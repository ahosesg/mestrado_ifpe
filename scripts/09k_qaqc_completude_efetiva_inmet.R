# ============================================================
# ETAPA 9K - QA/QC E COMPLETUDE EFETIVA DAS CANDIDATAS INMET
# Projeto de Mestrado - MP10 / Suape
# ============================================================
#
# OBJETIVO
# Reavaliar as candidatas automaticas identificadas na Etapa 9J
# usando completude efetiva em relacao ao calendario horario
# esperado, e nao apenas percentual de valores entre as linhas
# existentes no arquivo.
#
# PARA CADA ALVO MP10:
# - seleciona ate 3 automaticas mais proximas com as 4 variaveis
#   nucleares disponiveis;
# - adiciona a automatica de maior completude nuclear como
#   candidata diagnostica, caso ainda nao esteja entre as 3;
# - extrai os dados horarios brutos INMET;
# - aplica QA/QC fisico minimo e auditavel;
# - calcula completude horaria efetiva;
# - calcula contagem diaria de horas validas;
# - calcula sobreposicao com dias validos de MP10 para limiares
#   descritivos de 16, 18 e 20 horas.
#
# IMPORTANTE
# - 16/18/20 h sao sensibilidades descritivas nesta etapa, nao
#   criterio metodologico final;
# - temperatura nao recebe corte arbitrario de magnitude;
# - UR fora de 0-100%, vento <0, direcao fora de 0-360 graus e
#   pressao <=0 sao marcados como invalidos;
# - direcao do vento NAO e agregada por media aritmetica;
# - nenhuma estacao e escolhida automaticamente;
# - SUAPE 2022 continua sem pareamento espacial.
# ============================================================

if (!requireNamespace("data.table", quietly = TRUE)) {
  stop("Instale o pacote 'data.table' antes de executar.")
}
library(data.table)

# ------------------------------------------------------------
# 1. CAMINHOS
# ------------------------------------------------------------

PASTA_BRUTA <- "C:/Users/NIVEA/Documents/RStudio/Mestrado/precipitacao"

ARQ_TRIAGEM <- file.path(
  "outputs", "09_meteorologia", "multivariaveis_inmet",
  "triagem_espacial_temporal",
  "triagem_estacoes_automaticas.csv"
)

ARQ_MP10 <- file.path(
  "outputs", "04_completude",
  "completude_diaria_mp10.csv"
)

SAIDA <- file.path(
  "outputs", "09_meteorologia", "multivariaveis_inmet",
  "qaqc_completude_candidatas"
)

ARQ_RDS <- file.path(
  SAIDA,
  "base_horaria_inmet_candidatas_qaqc.rds"
)

dir.create(SAIDA, recursive = TRUE, showWarnings = FALSE)

for (f in c(ARQ_TRIAGEM, ARQ_MP10)) {
  if (!file.exists(f)) stop("Arquivo necessario nao encontrado: ", f)
}

if (!dir.exists(PASTA_BRUTA)) {
  stop("Pasta bruta nao encontrada: ", PASTA_BRUTA)
}

FUSO_INMET <- "UTC"
FUSO_LOCAL <- "America/Recife"

# ------------------------------------------------------------
# 2. FUNCOES AUXILIARES
# ------------------------------------------------------------

normalizar_texto <- function(x) {
  x <- as.character(x)
  x <- trimws(x)
  x[x %in% c("", "NA", "NULL", "null", "NaN")] <- NA_character_
  x
}

normalizar_chave <- function(x) {
  x <- iconv(as.character(x), from = "", to = "ASCII//TRANSLIT")
  x <- toupper(x)
  x <- gsub("[^A-Z0-9]+", "_", x)
  x <- gsub("^_+|_+$", "", x)
  x
}

para_numerico <- function(x) {
  if (is.numeric(x)) return(as.numeric(x))
  x <- normalizar_texto(x)
  x <- gsub(",", ".", x, fixed = TRUE)
  suppressWarnings(as.numeric(x))
}

parse_data_flexivel <- function(x) {
  x <- normalizar_texto(x)

  out <- as.Date(rep(NA_character_, length(x)))

  formatos <- c(
    "%Y-%m-%d",
    "%d/%m/%Y",
    "%Y/%m/%d",
    "%d-%m-%Y"
  )

  for (fmt in formatos) {
    idx <- is.na(out) & !is.na(x)
    if (!any(idx)) break
    out[idx] <- as.Date(x[idx], format = fmt)
  }

  out
}

montar_datahora_inmet <- function(data_txt, hora_txt, tz = "UTC") {

  datas <- parse_data_flexivel(data_txt)
  hora_txt <- normalizar_texto(hora_txt)

  hora_num <- suppressWarnings(
    as.integer(gsub("[^0-9]", "", hora_txt))
  )

  hhmm <- ifelse(
    is.na(hora_num),
    NA_character_,
    sprintf("%04d", hora_num)
  )

  eh_2400 <- !is.na(hhmm) & hhmm == "2400"

  hh <- suppressWarnings(as.integer(substr(hhmm, 1, 2)))
  mm <- suppressWarnings(as.integer(substr(hhmm, 3, 4)))

  hh[eh_2400] <- 0L
  mm[eh_2400] <- 0L

  datas[eh_2400 & !is.na(datas)] <-
    datas[eh_2400 & !is.na(datas)] + 1

  valido <- (
    !is.na(datas) &
      !is.na(hh) &
      !is.na(mm) &
      hh >= 0 & hh <= 23 &
      mm >= 0 & mm <= 59
  )

  txt <- rep(NA_character_, length(datas))

  txt[valido] <- sprintf(
    "%s %02d:%02d:00",
    format(datas[valido], "%Y-%m-%d"),
    hh[valido],
    mm[valido]
  )

  as.POSIXct(
    txt,
    format = "%Y-%m-%d %H:%M:%S",
    tz = tz
  )
}

converter_fuso <- function(x, fuso_destino) {
  as.POSIXct(
    format(
      x,
      tz = fuso_destino,
      format = "%Y-%m-%d %H:%M:%S"
    ),
    format = "%Y-%m-%d %H:%M:%S",
    tz = fuso_destino
  )
}

ler_linhas_inicio <- function(arquivo, n = 70L) {
  z <- tryCatch(
    readLines(
      arquivo,
      n = n,
      warn = FALSE,
      encoding = "UTF-8"
    ),
    error = function(e) character()
  )

  if (!length(z)) {
    z <- readLines(
      arquivo,
      n = n,
      warn = FALSE,
      encoding = "latin1"
    )
  }

  sub("^\ufeff", "", z)
}

valor_metadado_inmet <- function(linhas, chave) {
  chaves_norm <- normalizar_chave(sub(":.*$", "", linhas))
  alvo <- normalizar_chave(chave)
  idx <- which(chaves_norm == alvo)

  if (!length(idx)) return(NA_character_)

  normalizar_texto(
    sub("^[^:]+:\\s*", "", linhas[idx[1]])
  )
}

achar_indice <- function(
  nomes_norm,
  padroes_todos = NULL,
  padroes_um = NULL,
  excluir = NULL
) {
  ok <- rep(TRUE, length(nomes_norm))

  if (!is.null(padroes_todos)) {
    for (p in padroes_todos) {
      ok <- ok & grepl(p, nomes_norm)
    }
  }

  if (!is.null(padroes_um)) {
    ok_um <- rep(FALSE, length(nomes_norm))
    for (p in padroes_um) {
      ok_um <- ok_um | grepl(p, nomes_norm)
    }
    ok <- ok & ok_um
  }

  if (!is.null(excluir)) {
    for (p in excluir) {
      ok <- ok & !grepl(p, nomes_norm)
    }
  }

  idx <- which(ok)
  if (!length(idx)) return(NA_integer_)
  idx[1]
}

detectar_indices <- function(nomes_originais) {

  n <- normalizar_chave(nomes_originais)

  idx_data <- achar_indice(
    n,
    padroes_um = c("^DATA_MEDICAO$", "^DATA$")
  )

  idx_hora <- achar_indice(
    n,
    padroes_um = c("^HORA_MEDICAO$", "^HORA_UTC$", "^HORA$")
  )

  idx_temp <- achar_indice(
    n,
    padroes_todos = c("TEMPERATURA", "AR", "HORARIA"),
    excluir = c("MAXIMA", "MINIMA", "ORVALHO")
  )

  if (is.na(idx_temp)) {
    idx_temp <- achar_indice(
      n,
      padroes_todos = c("TEMPERATURA", "BULBO_SECO")
    )
  }

  idx_ur <- achar_indice(
    n,
    padroes_todos = c("UMIDADE", "RELATIVA", "HORARIA"),
    excluir = c("MAXIMA", "MINIMA")
  )

  idx_vel <- achar_indice(
    n,
    padroes_todos = c("VENTO", "VELOCIDADE"),
    excluir = c("RAJADA")
  )

  idx_dir <- achar_indice(
    n,
    padroes_todos = c("VENTO", "DIRE")
  )

  idx_pres <- achar_indice(
    n,
    padroes_todos = c(
      "PRESSAO", "ATMOSFERICA", "NIVEL", "ESTACAO"
    ),
    excluir = c("MAXIMA", "MINIMA")
  )

  c(
    data = idx_data,
    hora = idx_hora,
    temperatura = idx_temp,
    umidade = idx_ur,
    velocidade_vento = idx_vel,
    direcao_vento = idx_dir,
    pressao = idx_pres
  )
}

primeiro_nao_na <- function(x) {
  idx <- which(!is.na(x))

  if (length(idx)) {
    return(x[idx[1L]])
  }

  # Retorna NA preservando o tipo/classe original do vetor.
  x[NA_integer_][1L]
}

# ------------------------------------------------------------
# 3. SELECIONAR CANDIDATAS
# ------------------------------------------------------------

cat("\nCarregando triagem da Etapa 9J...\n")

triagem <- fread(
  ARQ_TRIAGEM,
  encoding = "UTF-8"
)

triagem[
  ,
  `:=`(
    cod_estacao = as.character(cod_estacao),
    ano_alvo = as.integer(ano_alvo),
    distancia_km = as.numeric(distancia_km),
    completude_min_nucleares = as.numeric(completude_min_nucleares)
  )
]

viaveis <- triagem[
  todas_nucleares_com_valor == TRUE
]

top3 <- viaveis[
  order(distancia_km),
  head(.SD, 3L),
  by = .(
    estacao_historica,
    ano_alvo
  )
]

melhor_comp <- viaveis[
  order(-completude_min_nucleares, distancia_km),
  head(.SD, 1L),
  by = .(
    estacao_historica,
    ano_alvo
  )
]

candidatos <- unique(
  rbindlist(
    list(top3, melhor_comp),
    fill = TRUE
  ),
  by = c(
    "estacao_historica",
    "ano_alvo",
    "cod_estacao"
  )
)

candidatos[
  ,
  origem_selecao := fifelse(
    rank_distancia <= 3 &
      rank_completude_nuclear == 1,
    "top3_distancia_e_melhor_completude",
    fifelse(
      rank_distancia <= 3,
      "top3_distancia",
      "melhor_completude"
    )
  )
]

setorder(
  candidatos,
  estacao_historica,
  ano_alvo,
  distancia_km
)

fwrite(
  candidatos,
  file.path(
    SAIDA,
    "candidatas_selecionadas_para_qaqc.csv"
  ),
  bom = TRUE
)

pares_estacao_ano <- unique(
  candidatos[
    ,
    .(
      cod_estacao,
      ano_alvo
    )
  ]
)

# ------------------------------------------------------------
# 4. LOCALIZAR E LER ARQUIVOS BRUTOS
# ------------------------------------------------------------

arquivos <- list.files(
  PASTA_BRUTA,
  pattern = "\\.csv$",
  full.names = TRUE,
  recursive = FALSE,
  ignore.case = TRUE
)

lista_base <- list()
lista_diag_arquivo <- list()

k <- 0L
kd <- 0L

codigos <- sort(unique(pares_estacao_ano$cod_estacao))

cat(
  "Estacoes automaticas selecionadas: ",
  paste(codigos, collapse = ", "),
  "\n",
  sep = ""
)

for (cod in codigos) {

  padrao <- paste0(
    "^dados_",
    cod,
    "_H_"
  )

  arqs_cod <- arquivos[
    grepl(
      padrao,
      basename(arquivos),
      ignore.case = TRUE
    )
  ]

  if (length(arqs_cod) != 1L) {
    kd <- kd + 1L

    lista_diag_arquivo[[kd]] <- data.table(
      cod_estacao = cod,
      arquivo = if (length(arqs_cod)) {
        paste(
          basename(arqs_cod),
          collapse = " | "
        )
      } else {
        NA_character_
      },
      status = "ERRO_LOCALIZACAO",
      mensagem = paste0(
        "Esperado 1 arquivo; encontrados ",
        length(arqs_cod)
      )
    )

    next
  }

  arquivo <- arqs_cod[1]

  cat("Lendo ", basename(arquivo), "...\n", sep = "")

  res <- tryCatch(
    {
      linhas <- ler_linhas_inicio(arquivo, 70L)
      linhas_norm <- normalizar_chave(linhas)

      idx_header <- which(
        grepl("^DATA_MEDICAO_", linhas_norm)
      )[1]

      if (is.na(idx_header)) {
        stop("Cabecalho Data Medicao nao encontrado.")
      }

      cod_meta <- valor_metadado_inmet(
        linhas,
        "Codigo Estacao"
      )

      nome_meta <- valor_metadado_inmet(
        linhas,
        "Nome"
      )

      lat_meta <- para_numerico(
        valor_metadado_inmet(
          linhas,
          "Latitude"
        )
      )

      lon_meta <- para_numerico(
        valor_metadado_inmet(
          linhas,
          "Longitude"
        )
      )

      raw <- fread(
        arquivo,
        skip = idx_header - 1L,
        sep = ";",
        dec = ",",
        encoding = "UTF-8",
        na.strings = c(
          "",
          "NA",
          "NULL",
          "null",
          "NaN"
        ),
        fill = TRUE,
        showProgress = FALSE
      )

      setnames(
        raw,
        names(raw),
        sub("^\ufeff", "", names(raw))
      )

      idx <- detectar_indices(
        names(raw)
      )

      if (any(is.na(idx))) {
        stop(
          "Uma ou mais colunas obrigatorias nao foram localizadas."
        )
      }

      dh_utc <- montar_datahora_inmet(
        as.character(raw[[idx["data"]]]),
        as.character(raw[[idx["hora"]]]),
        FUSO_INMET
      )

      dh_local <- converter_fuso(
        dh_utc,
        FUSO_LOCAL
      )

      x <- data.table(
        cod_estacao = as.character(cod_meta),
        nome_estacao = as.character(nome_meta),
        latitude = lat_meta,
        longitude = lon_meta,
        arquivo_origem = basename(arquivo),
        datahora_utc = dh_utc,
        datahora_local = dh_local,
        data_local = as.IDate(
          format(
            dh_local,
            "%Y-%m-%d"
          )
        ),
        ano_local = suppressWarnings(
          as.integer(
            format(
              dh_local,
              "%Y"
            )
          )
        ),
        hora_local = suppressWarnings(
          as.integer(
            format(
              dh_local,
              "%H"
            )
          )
        ),
        temperatura_ar_c = para_numerico(
          raw[[idx["temperatura"]]]
        ),
        umidade_relativa_pct = para_numerico(
          raw[[idx["umidade"]]]
        ),
        velocidade_vento_ms = para_numerico(
          raw[[idx["velocidade_vento"]]]
        ),
        direcao_vento_graus = para_numerico(
          raw[[idx["direcao_vento"]]]
        ),
        pressao_estacao_hpa = para_numerico(
          raw[[idx["pressao"]]]
        )
      )

      anos_cod <- pares_estacao_ano[
        cod_estacao == cod,
        unique(ano_alvo)
      ]

      x <- x[
        ano_local %in% anos_cod
      ]

      # Flags fisicos minimos.
      x[
        ,
        flag_ur_fora_0_100 := (
          !is.na(umidade_relativa_pct) &
            (
              umidade_relativa_pct < 0 |
                umidade_relativa_pct > 100
            )
        )
      ]

      x[
        ,
        flag_vel_vento_negativa := (
          !is.na(velocidade_vento_ms) &
            velocidade_vento_ms < 0
        )
      ]

      x[
        ,
        flag_dir_vento_fora_0_360 := (
          !is.na(direcao_vento_graus) &
            (
              direcao_vento_graus < 0 |
                direcao_vento_graus > 360
            )
        )
      ]

      x[
        ,
        flag_pressao_nao_positiva := (
          !is.na(pressao_estacao_hpa) &
            pressao_estacao_hpa <= 0
        )
      ]

      x[
        ,
        temperatura_valida := !is.na(
          temperatura_ar_c
        )
      ]

      x[
        ,
        umidade_valida := (
          !is.na(umidade_relativa_pct) &
            !flag_ur_fora_0_100
        )
      ]

      x[
        ,
        velocidade_vento_valida := (
          !is.na(velocidade_vento_ms) &
            !flag_vel_vento_negativa
        )
      ]

      x[
        ,
        direcao_vento_valida := (
          !is.na(direcao_vento_graus) &
            !flag_dir_vento_fora_0_360
        )
      ]

      x[
        ,
        pressao_valida := (
          !is.na(pressao_estacao_hpa) &
            !flag_pressao_nao_positiva
        )
      ]

      k <- k + 1L
      lista_base[[k]] <- x

      kd <- kd + 1L
      lista_diag_arquivo[[kd]] <- data.table(
        cod_estacao = cod,
        arquivo = basename(arquivo),
        status = "OK",
        mensagem = NA_character_
      )

      rm(raw, x, dh_utc, dh_local)
      gc()

      NULL
    },
    error = function(e) {
      kd <<- kd + 1L
      lista_diag_arquivo[[kd]] <<- data.table(
        cod_estacao = cod,
        arquivo = basename(arquivo),
        status = "ERRO_LEITURA",
        mensagem = conditionMessage(e)
      )
      NULL
    }
  )
}

diag_arquivos <- rbindlist(
  lista_diag_arquivo,
  fill = TRUE
)

fwrite(
  diag_arquivos,
  file.path(
    SAIDA,
    "diagnostico_leitura_arquivos.csv"
  ),
  bom = TRUE
)

if (!length(lista_base)) {
  stop("Nenhuma base horaria candidata foi produzida.")
}

base <- rbindlist(
  lista_base,
  fill = TRUE
)

# ------------------------------------------------------------
# 5. DUPLICATAS HORARIAS
# ------------------------------------------------------------

cat("Auditando duplicatas horarias...\n")

dup <- base[
  !is.na(datahora_local),
  .(
    n = .N,
    n_temp = uniqueN(
      temperatura_ar_c,
      na.rm = FALSE
    ),
    n_ur = uniqueN(
      umidade_relativa_pct,
      na.rm = FALSE
    ),
    n_vel = uniqueN(
      velocidade_vento_ms,
      na.rm = FALSE
    ),
    n_dir = uniqueN(
      direcao_vento_graus,
      na.rm = FALSE
    ),
    n_press = uniqueN(
      pressao_estacao_hpa,
      na.rm = FALSE
    )
  ),
  by = .(
    cod_estacao,
    datahora_local
  )
][
  n > 1
]

dup[
  ,
  conflito := (
    n_temp > 1 |
      n_ur > 1 |
      n_vel > 1 |
      n_dir > 1 |
      n_press > 1
  )
]

fwrite(
  dup,
  file.path(
    SAIDA,
    "diagnostico_duplicatas_horarias.csv"
  ),
  bom = TRUE
)

# Como os arquivos automaticos anuais devem ter um registro por hora,
# colapsa apenas duplicatas sem conflito. Em conflitos, as variaveis
# sao tornadas NA para impedir escolha silenciosa entre valores.
base[
  ,
  conflito_horario := FALSE
]

if (nrow(dup)) {

  chaves_conflito <- dup[
    conflito == TRUE,
    .(
      cod_estacao,
      datahora_local
    )
  ]

  if (nrow(chaves_conflito)) {
    base[
      chaves_conflito,
      on = .(
        cod_estacao,
        datahora_local
      ),
      conflito_horario := TRUE
    ]
  }
}

# Registros sem timestamp local permanecem apenas no diagnostico de QA/QC.
# Eles nao podem compor a base horaria analitica nem ser colapsados em um
# unico grupo NA por estacao.
base_tempo_valido <- base[
  !is.na(datahora_local)
]

base_unica <- base_tempo_valido[
  ,
  {
    confl <- any(conflito_horario)

    list(
      nome_estacao = primeiro_nao_na(
        nome_estacao
      ),
      latitude = primeiro_nao_na(
        latitude
      ),
      longitude = primeiro_nao_na(
        longitude
      ),
      arquivo_origem = primeiro_nao_na(
        arquivo_origem
      ),
      datahora_utc = primeiro_nao_na(
        datahora_utc
      ),
      data_local = primeiro_nao_na(
        data_local
      ),
      ano_local = primeiro_nao_na(
        ano_local
      ),
      hora_local = primeiro_nao_na(
        hora_local
      ),

      temperatura_ar_c = if (confl) {
        NA_real_
      } else {
        primeiro_nao_na(
          temperatura_ar_c
        )
      },

      umidade_relativa_pct = if (confl) {
        NA_real_
      } else {
        primeiro_nao_na(
          umidade_relativa_pct
        )
      },

      velocidade_vento_ms = if (confl) {
        NA_real_
      } else {
        primeiro_nao_na(
          velocidade_vento_ms
        )
      },

      direcao_vento_graus = if (confl) {
        NA_real_
      } else {
        primeiro_nao_na(
          direcao_vento_graus
        )
      },

      pressao_estacao_hpa = if (confl) {
        NA_real_
      } else {
        primeiro_nao_na(
          pressao_estacao_hpa
        )
      },

      temperatura_valida = if (confl) {
        FALSE
      } else {
        any(
          temperatura_valida %in% TRUE
        )
      },

      umidade_valida = if (confl) {
        FALSE
      } else {
        any(
          umidade_valida %in% TRUE
        )
      },

      velocidade_vento_valida = if (confl) {
        FALSE
      } else {
        any(
          velocidade_vento_valida %in% TRUE
        )
      },

      direcao_vento_valida = if (confl) {
        FALSE
      } else {
        any(
          direcao_vento_valida %in% TRUE
        )
      },

      pressao_valida = if (confl) {
        FALSE
      } else {
        any(
          pressao_valida %in% TRUE
        )
      },

      conflito_horario = confl
    )
  },
  by = .(
    cod_estacao,
    datahora_local
  )
]

saveRDS(
  base_unica,
  ARQ_RDS,
  compress = "xz"
)

# ------------------------------------------------------------
# 6. QA/QC RESUMO
# ------------------------------------------------------------

qaqc <- base[
  ,
  .(
    n_linhas = .N,
    n_datahora_ausente = sum(
      is.na(datahora_local)
    ),
    n_ur_fora_0_100 = sum(
      flag_ur_fora_0_100,
      na.rm = TRUE
    ),
    n_vel_vento_negativa = sum(
      flag_vel_vento_negativa,
      na.rm = TRUE
    ),
    n_dir_vento_fora_0_360 = sum(
      flag_dir_vento_fora_0_360,
      na.rm = TRUE
    ),
    n_pressao_nao_positiva = sum(
      flag_pressao_nao_positiva,
      na.rm = TRUE
    ),
    temperatura_min = if (
      all(is.na(temperatura_ar_c))
    ) NA_real_ else min(
      temperatura_ar_c,
      na.rm = TRUE
    ),
    temperatura_max = if (
      all(is.na(temperatura_ar_c))
    ) NA_real_ else max(
      temperatura_ar_c,
      na.rm = TRUE
    ),
    ur_min = if (
      all(is.na(umidade_relativa_pct))
    ) NA_real_ else min(
      umidade_relativa_pct,
      na.rm = TRUE
    ),
    ur_max = if (
      all(is.na(umidade_relativa_pct))
    ) NA_real_ else max(
      umidade_relativa_pct,
      na.rm = TRUE
    ),
    vel_vento_max = if (
      all(is.na(velocidade_vento_ms))
    ) NA_real_ else max(
      velocidade_vento_ms,
      na.rm = TRUE
    ),
    pressao_min = if (
      all(is.na(pressao_estacao_hpa))
    ) NA_real_ else min(
      pressao_estacao_hpa,
      na.rm = TRUE
    ),
    pressao_max = if (
      all(is.na(pressao_estacao_hpa))
    ) NA_real_ else max(
      pressao_estacao_hpa,
      na.rm = TRUE
    )
  ),
  by = .(
    cod_estacao,
    ano_local
  )
]

fwrite(
  qaqc,
  file.path(
    SAIDA,
    "resumo_qaqc_candidatas.csv"
  ),
  bom = TRUE
)

# ------------------------------------------------------------
# 7. COMPLETUDE HORARIA EFETIVA
# ------------------------------------------------------------

cat("Calculando completude horaria efetiva...\n")

resumo_horario <- base_unica[
  ,
  {
    ano_ref <- unique(ano_local)

    inicio <- as.POSIXct(
      sprintf(
        "%04d-01-01 00:00:00",
        ano_ref
      ),
      tz = FUSO_LOCAL
    )

    fim <- as.POSIXct(
      sprintf(
        "%04d-12-31 23:00:00",
        ano_ref
      ),
      tz = FUSO_LOCAL
    )

    n_esperado <- length(
      seq(
        inicio,
        fim,
        by = "hour"
      )
    )

    list(
      n_horas_esperadas = n_esperado,
      n_timestamps_presentes = uniqueN(
        datahora_local[
          !is.na(datahora_local)
        ]
      ),
      n_temp_validas = sum(
        temperatura_valida %in% TRUE
      ),
      n_ur_validas = sum(
        umidade_valida %in% TRUE
      ),
      n_vel_validas = sum(
        velocidade_vento_valida %in% TRUE
      ),
      n_dir_validas = sum(
        direcao_vento_valida %in% TRUE
      ),
      n_press_validas = sum(
        pressao_valida %in% TRUE
      ),
      n_conflitos_horarios = sum(
        conflito_horario %in% TRUE
      )
    )
  },
  by = .(
    cod_estacao,
    nome_estacao,
    ano_local
  )
]

resumo_horario[
  ,
  `:=`(
    pct_timestamps_presentes =
      100 *
        n_timestamps_presentes /
        n_horas_esperadas,

    pct_temp_efetiva =
      100 *
        n_temp_validas /
        n_horas_esperadas,

    pct_ur_efetiva =
      100 *
        n_ur_validas /
        n_horas_esperadas,

    pct_vel_efetiva =
      100 *
        n_vel_validas /
        n_horas_esperadas,

    pct_dir_efetiva =
      100 *
        n_dir_validas /
        n_horas_esperadas,

    pct_press_efetiva =
      100 *
        n_press_validas /
        n_horas_esperadas
  )
]

resumo_horario[
  ,
  completude_min_nuclear_efetiva := pmin(
    pct_temp_efetiva,
    pct_ur_efetiva,
    pct_vel_efetiva,
    pct_dir_efetiva,
    na.rm = FALSE
  )
]

fwrite(
  resumo_horario,
  file.path(
    SAIDA,
    "completude_horaria_efetiva_candidatas.csv"
  ),
  bom = TRUE
)

# ------------------------------------------------------------
# 8. COMPLETUDE DIARIA, SEM AGREGAR VALORES METEOROLOGICOS
# ------------------------------------------------------------

cat("Calculando contagem diaria de horas validas...\n")

diaria <- base_unica[
  ,
  .(
    n_horas_timestamp = uniqueN(
      datahora_local[
        !is.na(datahora_local)
      ]
    ),
    n_temp_validas = sum(
      temperatura_valida %in% TRUE
    ),
    n_ur_validas = sum(
      umidade_valida %in% TRUE
    ),
    n_vel_validas = sum(
      velocidade_vento_valida %in% TRUE
    ),
    n_dir_validas = sum(
      direcao_vento_valida %in% TRUE
    ),
    n_press_validas = sum(
      pressao_valida %in% TRUE
    )
  ),
  by = .(
    cod_estacao,
    nome_estacao,
    ano_local,
    data_local
  )
]

for (lim in c(16L, 18L, 20L)) {

  diaria[
    ,
    paste0(
      "temp_",
      lim,
      "h"
    ) := n_temp_validas >= lim
  ]

  diaria[
    ,
    paste0(
      "ur_",
      lim,
      "h"
    ) := n_ur_validas >= lim
  ]

  diaria[
    ,
    paste0(
      "vel_",
      lim,
      "h"
    ) := n_vel_validas >= lim
  ]

  diaria[
    ,
    paste0(
      "dir_",
      lim,
      "h"
    ) := n_dir_validas >= lim
  ]

  diaria[
    ,
    paste0(
      "nuclear_",
      lim,
      "h"
    ) := (
      n_temp_validas >= lim &
        n_ur_validas >= lim &
        n_vel_validas >= lim &
        n_dir_validas >= lim
    )
  ]
}

fwrite(
  diaria,
  file.path(
    SAIDA,
    "completude_diaria_horas_validas_candidatas.csv"
  ),
  bom = TRUE
)

# ------------------------------------------------------------
# 9. SOBREPOSICAO COM DIAS VALIDOS DE MP10
# ------------------------------------------------------------

cat("Calculando sobreposicao com dias validos de MP10...\n")

mp10 <- fread(
  ARQ_MP10,
  encoding = "UTF-8"
)

mp10[
  ,
  `:=`(
    ano = as.integer(ano),
    data_local = as.IDate(data),
    estacao_historica = as.character(no_estacao)
  )
]

alvos <- unique(
  candidatos[
    ,
    .(
      estacao_historica,
      ano_alvo,
      cod_estacao,
      nome_estacao,
      distancia_km,
      origem_selecao,
      completude_min_nucleares_09j =
        completude_min_nucleares
    )
  ]
)

# Evita colisao do campo nome_estacao no merge.
# O nome oficial da candidata ja esta em `alvos`; em `diaria` ele e
# redundante e geraria nome_estacao.x / nome_estacao.y.
diaria_join <- copy(diaria)
diaria_join[
  ,
  nome_estacao := NULL
]

grade_alvos <- merge(
  alvos,
  diaria_join,
  by.x = c(
    "cod_estacao",
    "ano_alvo"
  ),
  by.y = c(
    "cod_estacao",
    "ano_local"
  ),
  all.x = TRUE,
  allow.cartesian = TRUE
)

grade_alvos <- merge(
  grade_alvos,
  mp10[
    ,
    .(
      estacao_historica,
      ano_alvo = ano,
      data_local,
      dia_mp10_valido = dia_valido_mma_16h
    )
  ],
  by = c(
    "estacao_historica",
    "ano_alvo",
    "data_local"
  ),
  all.x = TRUE
)

campos_obrigatorios_grade <- c(
  "estacao_historica",
  "ano_alvo",
  "cod_estacao",
  "nome_estacao",
  "distancia_km",
  "origem_selecao",
  "completude_min_nucleares_09j",
  "dia_mp10_valido",
  "nuclear_16h",
  "nuclear_18h",
  "nuclear_20h"
)

faltantes_grade <- setdiff(
  campos_obrigatorios_grade,
  names(grade_alvos)
)

if (length(faltantes_grade)) {
  stop(
    "Colunas obrigatorias ausentes em grade_alvos: ",
    paste(
      faltantes_grade,
      collapse = ", "
    )
  )
}

# O denominador de dias validos de MP10 deve ser independente da
# disponibilidade da candidata meteorologica. Caso contrario, uma
# estacao com meses inteiros ausentes teria seu percentual de
# sobreposicao artificialmente inflado.
denominadores_mp10 <- mp10[
  dia_valido_mma_16h %in% TRUE,
  .(
    n_dias_mp10_validos = uniqueN(
      data_local
    )
  ),
  by = .(
    estacao_historica,
    ano_alvo = ano
  )
]

sobreposicao <- grade_alvos[
  ,
  .(
    n_overlap_nuclear_16h = sum(
      dia_mp10_valido %in% TRUE &
        nuclear_16h %in% TRUE,
      na.rm = TRUE
    ),

    n_overlap_nuclear_18h = sum(
      dia_mp10_valido %in% TRUE &
        nuclear_18h %in% TRUE,
      na.rm = TRUE
    ),

    n_overlap_nuclear_20h = sum(
      dia_mp10_valido %in% TRUE &
        nuclear_20h %in% TRUE,
      na.rm = TRUE
    )
  ),
  by = .(
    estacao_historica,
    ano_alvo,
    cod_estacao,
    nome_estacao,
    distancia_km,
    origem_selecao,
    completude_min_nucleares_09j
  )
]

sobreposicao <- merge(
  sobreposicao,
  denominadores_mp10,
  by = c(
    "estacao_historica",
    "ano_alvo"
  ),
  all.x = TRUE
)

if (any(is.na(sobreposicao$n_dias_mp10_validos))) {
  stop(
    "Foi impossivel recuperar o denominador completo de dias validos ",
    "de MP10 para uma ou mais combinacoes alvo-candidata."
  )
}

sobreposicao[
  ,
  `:=`(
    pct_overlap_nuclear_16h =
      fifelse(
        n_dias_mp10_validos > 0,
        100 *
          n_overlap_nuclear_16h /
          n_dias_mp10_validos,
        NA_real_
      ),

    pct_overlap_nuclear_18h =
      fifelse(
        n_dias_mp10_validos > 0,
        100 *
          n_overlap_nuclear_18h /
          n_dias_mp10_validos,
        NA_real_
      ),

    pct_overlap_nuclear_20h =
      fifelse(
        n_dias_mp10_validos > 0,
        100 *
          n_overlap_nuclear_20h /
          n_dias_mp10_validos,
        NA_real_
      )
  )
]

sobreposicao <- merge(
  sobreposicao,
  resumo_horario[
    ,
    .(
      cod_estacao,
      ano_alvo = ano_local,
      completude_min_nuclear_efetiva,
      pct_temp_efetiva,
      pct_ur_efetiva,
      pct_vel_efetiva,
      pct_dir_efetiva,
      pct_press_efetiva,
      pct_timestamps_presentes
    )
  ],
  by = c(
    "cod_estacao",
    "ano_alvo"
  ),
  all.x = TRUE
)

# Auditoria: todas as candidatas do mesmo alvo devem compartilhar
# exatamente o mesmo denominador de dias validos de MP10.
aud_den <- sobreposicao[
  ,
  .(
    n_denominadores = uniqueN(
      n_dias_mp10_validos
    ),
    denominador = unique(
      n_dias_mp10_validos
    )[1]
  ),
  by = .(
    estacao_historica,
    ano_alvo
  )
]

if (any(aud_den$n_denominadores != 1L)) {
  stop(
    "Inconsistencia: candidatas do mesmo alvo possuem denominadores ",
    "de MP10 diferentes."
  )
}

fwrite(
  aud_den,
  file.path(
    SAIDA,
    "auditoria_denominador_mp10.csv"
  ),
  bom = TRUE
)

setorder(
  sobreposicao,
  estacao_historica,
  ano_alvo,
  distancia_km
)

fwrite(
  sobreposicao,
  file.path(
    SAIDA,
    "sobreposicao_mp10_multivariaveis_candidatas.csv"
  ),
  bom = TRUE
)

# ------------------------------------------------------------
# 10. RESUMO EXECUCAO
# ------------------------------------------------------------

resumo_exec <- data.table(
  indicador = c(
    "n_alvos_mp10",
    "n_combinacoes_alvo_candidata",
    "n_estacoes_automaticas_distintas",
    "n_estacao_ano_extraidos",
    "n_linhas_horarias_unicas",
    "n_grupos_duplicados",
    "n_grupos_duplicados_conflitantes",
    "n_linhas_sem_datahora_local",
    "n_ur_fora_0_100",
    "n_vel_vento_negativa",
    "n_dir_vento_fora_0_360",
    "n_pressao_nao_positiva"
  ),
  valor = c(
    uniqueN(
      candidatos[
        ,
        paste(
          estacao_historica,
          ano_alvo
        )
      ]
    ),
    nrow(candidatos),
    uniqueN(candidatos$cod_estacao),
    nrow(pares_estacao_ano),
    nrow(base_unica),
    nrow(dup),
    sum(
      dup$conflito %in% TRUE
    ),
    sum(
      is.na(base$datahora_local)
    ),
    sum(
      qaqc$n_ur_fora_0_100,
      na.rm = TRUE
    ),
    sum(
      qaqc$n_vel_vento_negativa,
      na.rm = TRUE
    ),
    sum(
      qaqc$n_dir_vento_fora_0_360,
      na.rm = TRUE
    ),
    sum(
      qaqc$n_pressao_nao_positiva,
      na.rm = TRUE
    )
  )
)

fwrite(
  resumo_exec,
  file.path(
    SAIDA,
    "resumo_execucao_qaqc_completude_candidatas.csv"
  ),
  bom = TRUE
)

cat("\n============================================================\n")
cat("ETAPA 9K - QA/QC E COMPLETUDE EFETIVA INMET CONCLUIDA\n")
cat("============================================================\n\n")

print(resumo_exec)

cat("\nCompletude efetiva e sobreposicao com MP10:\n")

print(
  sobreposicao[
    ,
    .(
      estacao_historica,
      ano_alvo,
      cod_estacao,
      nome_estacao,
      distancia_km,
      origem_selecao,
      completude_min_nucleares_09j,
      completude_min_nuclear_efetiva,
      pct_overlap_nuclear_16h,
      pct_overlap_nuclear_18h,
      pct_overlap_nuclear_20h
    )
  ]
)

cat(
  "\nIMPORTANTE:\n",
  "- a base horaria RDS e derivada e deve permanecer fora do Git;\n",
  "- 16/18/20 h sao sensibilidades, nao criterio final;\n",
  "- esta etapa testa a completude efetiva antes de decidir a fonte meteorologica final;\n",
  "- sera utilizado apenas INMET; menor completude ou maior distancia serao tratadas como limitacoes metodologicas.\n",
  sep = ""
)
