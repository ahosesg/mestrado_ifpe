# ============================================================
# ETAPA 9I - INVENTARIO MULTIVARIAVEL DAS ESTACOES INMET
# Projeto de Mestrado - MP10 / Suape
# Periodo: 2017-2025
# ============================================================
#
# OBJETIVO
# Inventariar, nos arquivos INMET ja utilizados na Etapa 9A, a
# disponibilidade de:
# - temperatura do ar;
# - umidade relativa do ar;
# - velocidade do vento;
# - direcao do vento;
# - pressao atmosferica ao nivel da estacao.
#
# ESTA ETAPA NAO:
# - seleciona a estacao meteorologica final;
# - interpola ou imputa dados;
# - agrega dados horarios em medias diarias;
# - trata direcao do vento como variavel linear;
# - mistura automaticamente estacoes automaticas e convencionais.
#
# SAIDAS
# 1. inventario dos arquivos INMET;
# 2. mapa de colunas detectadas;
# 3. disponibilidade por estacao-ano-variavel;
# 4. resumo por estacao;
# 5. diagnostico de arquivos/variaveis ausentes.
# ============================================================

if (!requireNamespace("data.table", quietly = TRUE)) {
  stop("Instale o pacote 'data.table' antes de executar.")
}
library(data.table)

# ------------------------------------------------------------
# 1. CONFIGURACOES
# ------------------------------------------------------------

PASTA_ENTRADA <- "C:/Users/NIVEA/Documents/RStudio/Mestrado/precipitacao"

PASTA_SAIDA <- file.path(
  "outputs",
  "09_meteorologia",
  "multivariaveis_inmet"
)

ANOS_ALVO <- 2017:2025

FUSO_INMET <- "UTC"
FUSO_LOCAL <- "America/Recife"

dir.create(PASTA_SAIDA, recursive = TRUE, showWarnings = FALSE)

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
  x <- iconv(
    as.character(x),
    from = "",
    to = "ASCII//TRANSLIT"
  )
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

  out <- as.Date(
    rep(
      NA_character_,
      length(x)
    )
  )

  formatos <- c(
    "%Y-%m-%d",
    "%d/%m/%Y",
    "%Y/%m/%d",
    "%d-%m-%Y"
  )

  for (fmt in formatos) {
    idx <- is.na(out) & !is.na(x)
    if (!any(idx)) break

    out[idx] <- as.Date(
      x[idx],
      format = fmt
    )
  }

  out
}

montar_datahora_inmet <- function(
  data_txt,
  hora_txt,
  tz = "UTC"
) {

  datas <- parse_data_flexivel(
    data_txt
  )

  hora_txt <- normalizar_texto(
    hora_txt
  )

  hora_num <- suppressWarnings(
    as.integer(
      gsub(
        "[^0-9]",
        "",
        hora_txt
      )
    )
  )

  hhmm <- ifelse(
    is.na(hora_num),
    NA_character_,
    sprintf(
      "%04d",
      hora_num
    )
  )

  eh_2400 <- (
    !is.na(hhmm) &
      hhmm == "2400"
  )

  hh <- suppressWarnings(
    as.integer(
      substr(
        hhmm,
        1,
        2
      )
    )
  )

  mm <- suppressWarnings(
    as.integer(
      substr(
        hhmm,
        3,
        4
      )
    )
  )

  hh[eh_2400] <- 0L
  mm[eh_2400] <- 0L

  datas[
    eh_2400 &
      !is.na(datas)
  ] <- datas[
    eh_2400 &
      !is.na(datas)
  ] + 1

  valido <- (
    !is.na(datas) &
      !is.na(hh) &
      !is.na(mm) &
      hh >= 0 &
      hh <= 23 &
      mm >= 0 &
      mm <= 59
  )

  texto <- rep(
    NA_character_,
    length(datas)
  )

  texto[valido] <- sprintf(
    "%s %02d:%02d:00",
    format(
      datas[valido],
      "%Y-%m-%d"
    ),
    hh[valido],
    mm[valido]
  )

  as.POSIXct(
    texto,
    format = "%Y-%m-%d %H:%M:%S",
    tz = tz
  )
}

converter_fuso <- function(
  x,
  fuso_destino
) {

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

ler_linhas_inicio <- function(
  arquivo,
  n = 70L
) {

  z <- tryCatch(
    readLines(
      arquivo,
      n = n,
      warn = FALSE,
      encoding = "UTF-8"
    ),
    error = function(e) character()
  )

  if (length(z) == 0) {
    z <- readLines(
      arquivo,
      n = n,
      warn = FALSE,
      encoding = "latin1"
    )
  }

  sub(
    "^\ufeff",
    "",
    z
  )
}

valor_metadado_inmet <- function(
  linhas,
  chave
) {

  chaves_norm <- normalizar_chave(
    sub(
      ":.*$",
      "",
      linhas
    )
  )

  alvo <- normalizar_chave(
    chave
  )

  idx <- which(
    chaves_norm == alvo
  )

  if (length(idx) == 0) {
    return(
      NA_character_
    )
  }

  normalizar_texto(
    sub(
      "^[^:]+:\\s*",
      "",
      linhas[idx[1]]
    )
  )
}

detectar_inmet <- function(
  arquivo
) {

  linhas <- ler_linhas_inicio(
    arquivo,
    n = 40L
  )

  if (!length(linhas)) {
    return(FALSE)
  }

  z <- normalizar_chave(
    linhas
  )

  any(
    grepl(
      "^NOME_",
      z
    )
  ) &&
    any(
      grepl(
        "^CODIGO_ESTACAO_",
        z
      )
    ) &&
    any(
      grepl(
        "^DATA_MEDICAO_",
        z
      )
    )
}

achar_indice <- function(
  nomes_norm,
  padroes_todos = NULL,
  padroes_um = NULL,
  excluir = NULL
) {

  ok <- rep(
    TRUE,
    length(nomes_norm)
  )

  if (!is.null(padroes_todos)) {
    for (p in padroes_todos) {
      ok <- ok &
        grepl(
          p,
          nomes_norm
        )
    }
  }

  if (!is.null(padroes_um)) {
    ok_um <- rep(
      FALSE,
      length(nomes_norm)
    )

    for (p in padroes_um) {
      ok_um <- ok_um |
        grepl(
          p,
          nomes_norm
        )
    }

    ok <- ok & ok_um
  }

  if (!is.null(excluir)) {
    for (p in excluir) {
      ok <- ok &
        !grepl(
          p,
          nomes_norm
        )
    }
  }

  idx <- which(ok)

  if (!length(idx)) {
    return(
      NA_integer_
    )
  }

  idx[1]
}

detectar_colunas_alvo <- function(
  nomes_originais
) {

  n <- normalizar_chave(
    nomes_originais
  )

  idx_data <- achar_indice(
    n,
    padroes_um = c(
      "^DATA_MEDICAO$",
      "^DATA$"
    )
  )

  idx_hora <- achar_indice(
    n,
    padroes_um = c(
      "^HORA_MEDICAO$",
      "^HORA_UTC$",
      "^HORA$"
    )
  )

  idx_temp <- achar_indice(
    n,
    padroes_todos = c(
      "TEMPERATURA",
      "AR",
      "HORARIA"
    ),
    excluir = c(
      "MAXIMA",
      "MINIMA",
      "ORVALHO"
    )
  )

  # Fallback muito comum nos arquivos automaticos anuais:
  # TEMPERATURA DO AR - BULBO SECO, HORARIA
  if (is.na(idx_temp)) {
    idx_temp <- achar_indice(
      n,
      padroes_todos = c(
        "TEMPERATURA",
        "BULBO_SECO"
      )
    )
  }

  idx_ur <- achar_indice(
    n,
    padroes_todos = c(
      "UMIDADE",
      "RELATIVA",
      "HORARIA"
    ),
    excluir = c(
      "MAXIMA",
      "MINIMA"
    )
  )

  idx_vel_vento <- achar_indice(
    n,
    padroes_todos = c(
      "VENTO",
      "VELOCIDADE"
    ),
    excluir = c(
      "RAJADA"
    )
  )

  idx_dir_vento <- achar_indice(
    n,
    padroes_todos = c(
      "VENTO",
      "DIRE"
    )
  )

  idx_pressao <- achar_indice(
    n,
    padroes_todos = c(
      "PRESSAO",
      "ATMOSFERICA",
      "NIVEL",
      "ESTACAO"
    ),
    excluir = c(
      "MAXIMA",
      "MINIMA"
    )
  )

  data.table(
    variavel = c(
      "temperatura_ar_c",
      "umidade_relativa_pct",
      "velocidade_vento_ms",
      "direcao_vento_graus",
      "pressao_estacao_hpa"
    ),
    indice = c(
      idx_temp,
      idx_ur,
      idx_vel_vento,
      idx_dir_vento,
      idx_pressao
    ),
    coluna_original = c(
      ifelse(
        is.na(idx_temp),
        NA_character_,
        nomes_originais[idx_temp]
      ),
      ifelse(
        is.na(idx_ur),
        NA_character_,
        nomes_originais[idx_ur]
      ),
      ifelse(
        is.na(idx_vel_vento),
        NA_character_,
        nomes_originais[idx_vel_vento]
      ),
      ifelse(
        is.na(idx_dir_vento),
        NA_character_,
        nomes_originais[idx_dir_vento]
      ),
      ifelse(
        is.na(idx_pressao),
        NA_character_,
        nomes_originais[idx_pressao]
      )
    ),
    idx_data = idx_data,
    idx_hora = idx_hora
  )
}

resumo_num <- function(x) {

  x <- as.numeric(x)

  data.table(
    n_total = length(x),
    n_validos = sum(
      !is.na(x)
    ),
    n_ausentes = sum(
      is.na(x)
    ),
    minimo = if (
      all(
        is.na(x)
      )
    ) {
      NA_real_
    } else {
      min(
        x,
        na.rm = TRUE
      )
    },
    p01 = if (
      all(
        is.na(x)
      )
    ) {
      NA_real_
    } else {
      as.numeric(
        quantile(
          x,
          0.01,
          na.rm = TRUE,
          names = FALSE
        )
      )
    },
    mediana = if (
      all(
        is.na(x)
      )
    ) {
      NA_real_
    } else {
      median(
        x,
        na.rm = TRUE
      )
    },
    p99 = if (
      all(
        is.na(x)
      )
    ) {
      NA_real_
    } else {
      as.numeric(
        quantile(
          x,
          0.99,
          na.rm = TRUE,
          names = FALSE
        )
      )
    },
    maximo = if (
      all(
        is.na(x)
      )
    ) {
      NA_real_
    } else {
      max(
        x,
        na.rm = TRUE
      )
    }
  )
}

# ------------------------------------------------------------
# 3. LOCALIZAR ARQUIVOS
# ------------------------------------------------------------

arquivos <- list.files(
  PASTA_ENTRADA,
  pattern = "\\.csv$",
  full.names = TRUE,
  recursive = FALSE,
  ignore.case = TRUE
)

if (!length(arquivos)) {
  stop(
    "Nenhum CSV encontrado em: ",
    PASTA_ENTRADA
  )
}

eh_inmet <- vapply(
  arquivos,
  detectar_inmet,
  logical(1)
)

arquivos_inmet <- arquivos[
  eh_inmet
]

cat(
  "\nArquivos CSV totais: ",
  length(arquivos),
  "\n",
  "Arquivos INMET detectados: ",
  length(arquivos_inmet),
  "\n\n",
  sep = ""
)

if (!length(arquivos_inmet)) {
  stop(
    "Nenhum arquivo INMET foi detectado."
  )
}

# ------------------------------------------------------------
# 4. PROCESSAR ARQUIVOS INMET
# ------------------------------------------------------------

lista_arquivo <- list()
lista_colunas <- list()
lista_estacao_ano_variavel <- list()

k_arq <- 0L
k_col <- 0L
k_eav <- 0L

for (i in seq_along(arquivos_inmet)) {

  arquivo <- arquivos_inmet[i]

  cat(
    sprintf(
      "[%02d/%02d] %s\n",
      i,
      length(arquivos_inmet),
      basename(arquivo)
    )
  )

  resultado <- tryCatch(
    {

      linhas <- ler_linhas_inicio(
        arquivo,
        n = 70L
      )

      linhas_norm <- normalizar_chave(
        linhas
      )

      idx_header <- which(
        grepl(
          "^DATA_MEDICAO_",
          linhas_norm
        )
      )[1]

      if (is.na(idx_header)) {
        stop(
          "Cabecalho Data Medicao nao encontrado."
        )
      }

      nome_estacao <- valor_metadado_inmet(
        linhas,
        "Nome"
      )

      cod_estacao <- valor_metadado_inmet(
        linhas,
        "Codigo Estacao"
      )

      latitude <- para_numerico(
        valor_metadado_inmet(
          linhas,
          "Latitude"
        )
      )

      longitude <- para_numerico(
        valor_metadado_inmet(
          linhas,
          "Longitude"
        )
      )

      altitude <- para_numerico(
        valor_metadado_inmet(
          linhas,
          "Altitude"
        )
      )

      situacao <- valor_metadado_inmet(
        linhas,
        "Situacao"
      )

      periodicidade <- valor_metadado_inmet(
        linhas,
        "Periodicidade da Medicao"
      )

      tipo_estacao <- if (
        !is.na(cod_estacao) &&
          grepl(
            "^A[0-9]+$",
            toupper(
              cod_estacao
            )
          )
      ) {
        "AUTOMATICA"
      } else {
        "CONVENCIONAL"
      }

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
        sub(
          "^\ufeff",
          "",
          names(raw)
        )
      )

      mapa <- detectar_colunas_alvo(
        names(raw)
      )

      idx_data <- mapa$idx_data[1]
      idx_hora <- mapa$idx_hora[1]

      if (
        is.na(idx_data) ||
          is.na(idx_hora)
      ) {
        stop(
          "Colunas de data/hora nao localizadas."
        )
      }

      dh_utc <- montar_datahora_inmet(
        as.character(
          raw[[idx_data]]
        ),
        as.character(
          raw[[idx_hora]]
        ),
        tz = FUSO_INMET
      )

      dh_local <- converter_fuso(
        dh_utc,
        FUSO_LOCAL
      )

      ano_local <- suppressWarnings(
        as.integer(
          format(
            dh_local,
            "%Y"
          )
        )
      )

      mes_local <- suppressWarnings(
        as.integer(
          format(
            dh_local,
            "%m"
          )
        )
      )

      data_local <- as.IDate(
        format(
          dh_local,
          "%Y-%m-%d"
        )
      )

      k_arq <- k_arq + 1L

      lista_arquivo[[k_arq]] <- data.table(
        arquivo = basename(
          arquivo
        ),
        status_leitura = "OK",
        mensagem = NA_character_,
        cod_estacao = normalizar_texto(
          cod_estacao
        ),
        nome_estacao = normalizar_texto(
          nome_estacao
        ),
        tipo_estacao = tipo_estacao,
        latitude = latitude,
        longitude = longitude,
        altitude_m = altitude,
        situacao_estacao = normalizar_texto(
          situacao
        ),
        periodicidade_fonte = normalizar_texto(
          periodicidade
        ),
        n_linhas = nrow(
          raw
        ),
        n_datahora_na = sum(
          is.na(
            dh_utc
          )
        ),
        inicio_utc = if (
          all(
            is.na(
              dh_utc
            )
          )
        ) {
          as.POSIXct(
            NA,
            tz = "UTC"
          )
        } else {
          min(
            dh_utc,
            na.rm = TRUE
          )
        },
        fim_utc = if (
          all(
            is.na(
              dh_utc
            )
          )
        ) {
          as.POSIXct(
            NA,
            tz = "UTC"
          )
        } else {
          max(
            dh_utc,
            na.rm = TRUE
          )
        }
      )

      for (
        j in seq_len(
          nrow(
            mapa
          )
        )
      ) {

        k_col <- k_col + 1L

        lista_colunas[[k_col]] <- data.table(
          arquivo = basename(
            arquivo
          ),
          cod_estacao = normalizar_texto(
            cod_estacao
          ),
          nome_estacao = normalizar_texto(
            nome_estacao
          ),
          tipo_estacao = tipo_estacao,
          variavel = mapa$variavel[j],
          coluna_detectada = mapa$coluna_original[j],
          coluna_encontrada = !is.na(
            mapa$indice[j]
          )
        )

        if (
          is.na(
            mapa$indice[j]
          )
        ) {
          next
        }

        valores <- para_numerico(
          raw[[mapa$indice[j]]]
        )

        anos_presentes <- sort(
          unique(
            ano_local[
              !is.na(
                ano_local
              ) &
                ano_local %in% ANOS_ALVO
            ]
          )
        )

        if (!length(anos_presentes)) {
          next
        }

        for (aa in anos_presentes) {

          idx_ano <- (
            ano_local == aa &
              !is.na(
                ano_local
              )
          )

          z <- valores[
            idx_ano
          ]

          dh_a <- dh_local[
            idx_ano
          ]

          datas_a <- data_local[
            idx_ano
          ]

          meses_a <- mes_local[
            idx_ano
          ]

          res <- resumo_num(
            z
          )

          k_eav <- k_eav + 1L

          lista_estacao_ano_variavel[[k_eav]] <- data.table(
            arquivo = basename(
              arquivo
            ),
            cod_estacao = normalizar_texto(
              cod_estacao
            ),
            nome_estacao = normalizar_texto(
              nome_estacao
            ),
            tipo_estacao = tipo_estacao,
            latitude = latitude,
            longitude = longitude,
            altitude_m = altitude,
            periodicidade_fonte = normalizar_texto(
              periodicidade
            ),
            ano = aa,
            variavel = mapa$variavel[j],
            coluna_detectada = mapa$coluna_original[j],
            n_registros = length(
              z
            ),
            n_validos = res$n_validos,
            n_ausentes = res$n_ausentes,
            pct_validos = if (
              length(
                z
              ) > 0
            ) {
              100 *
                res$n_validos /
                length(
                  z
                )
            } else {
              NA_real_
            },
            n_datas_com_registro = uniqueN(
              datas_a[
                !is.na(
                  datas_a
                )
              ]
            ),
            n_datas_com_valor = uniqueN(
              datas_a[
                !is.na(
                  datas_a
                ) &
                  !is.na(
                    z
                  )
              ]
            ),
            n_meses_com_valor = uniqueN(
              meses_a[
                !is.na(
                  meses_a
                ) &
                  !is.na(
                    z
                  )
              ]
            ),
            inicio_local = if (
              all(
                is.na(
                  dh_a
                )
              )
            ) {
              as.POSIXct(
                NA,
                tz = FUSO_LOCAL
              )
            } else {
              min(
                dh_a,
                na.rm = TRUE
              )
            },
            fim_local = if (
              all(
                is.na(
                  dh_a
                )
              )
            ) {
              as.POSIXct(
                NA,
                tz = FUSO_LOCAL
              )
            } else {
              max(
                dh_a,
                na.rm = TRUE
              )
            },
            minimo = res$minimo,
            p01 = res$p01,
            mediana = res$mediana,
            p99 = res$p99,
            maximo = res$maximo
          )
        }
      }

      rm(
        raw,
        dh_utc,
        dh_local,
        ano_local,
        mes_local,
        data_local
      )

      gc()

      NULL
    },
    error = function(e) {

      k_arq <<- k_arq + 1L

      lista_arquivo[[k_arq]] <<- data.table(
        arquivo = basename(
          arquivo
        ),
        status_leitura = "ERRO",
        mensagem = conditionMessage(
          e
        ),
        cod_estacao = NA_character_,
        nome_estacao = NA_character_,
        tipo_estacao = NA_character_,
        latitude = NA_real_,
        longitude = NA_real_,
        altitude_m = NA_real_,
        situacao_estacao = NA_character_,
        periodicidade_fonte = NA_character_,
        n_linhas = NA_integer_,
        n_datahora_na = NA_integer_,
        inicio_utc = as.POSIXct(
          NA,
          tz = "UTC"
        ),
        fim_utc = as.POSIXct(
          NA,
          tz = "UTC"
        )
      )

      NULL
    }
  )
}

# ------------------------------------------------------------
# 5. CONSOLIDAR SAIDAS
# ------------------------------------------------------------

inventario_arquivos <- rbindlist(
  lista_arquivo,
  fill = TRUE
)

mapa_colunas <- rbindlist(
  lista_colunas,
  fill = TRUE
)

disponibilidade <- rbindlist(
  lista_estacao_ano_variavel,
  fill = TRUE
)

setorder(
  inventario_arquivos,
  tipo_estacao,
  cod_estacao,
  arquivo
)

setorder(
  mapa_colunas,
  tipo_estacao,
  cod_estacao,
  variavel
)

setorder(
  disponibilidade,
  tipo_estacao,
  cod_estacao,
  ano,
  variavel
)

# ------------------------------------------------------------
# 6. RESUMO POR ESTACAO
# ------------------------------------------------------------

resumo_estacoes <- disponibilidade[
  ,
  .(
    nome_estacao = {
      z <- unique(
        na.omit(
          nome_estacao
        )
      )
      if (!length(z)) {
        NA_character_
      } else {
        z[1]
      }
    },
    tipo_estacao = {
      z <- unique(
        na.omit(
          tipo_estacao
        )
      )
      if (!length(z)) {
        NA_character_
      } else {
        z[1]
      }
    },
    latitude = {
      z <- unique(
        na.omit(
          latitude
        )
      )
      if (!length(z)) {
        NA_real_
      } else {
        z[1]
      }
    },
    longitude = {
      z <- unique(
        na.omit(
          longitude
        )
      )
      if (!length(z)) {
        NA_real_
      } else {
        z[1]
      }
    },
    altitude_m = {
      z <- unique(
        na.omit(
          altitude_m
        )
      )
      if (!length(z)) {
        NA_real_
      } else {
        z[1]
      }
    },
    primeiro_ano = min(
      ano,
      na.rm = TRUE
    ),
    ultimo_ano = max(
      ano,
      na.rm = TRUE
    ),
    anos = paste(
      sort(
        unique(
          ano
        )
      ),
      collapse = ";"
    ),
    n_variaveis_detectadas = uniqueN(
      variavel[
        n_registros > 0
      ]
    ),
    n_variaveis_com_valor = uniqueN(
      variavel[
        n_validos > 0
      ]
    )
  ),
  by = .(
    cod_estacao
  )
]

# ------------------------------------------------------------
# 7. MATRIZ DE DISPONIBILIDADE NOS ANOS-ALVO
# ------------------------------------------------------------

matriz <- dcast(
  disponibilidade[
    ,
    .(
      cod_estacao,
      nome_estacao,
      tipo_estacao,
      ano,
      variavel,
      pct_validos
    )
  ],
  cod_estacao +
    nome_estacao +
    tipo_estacao +
    ano ~ variavel,
  value.var = "pct_validos"
)

# ------------------------------------------------------------
# 8. DIAGNOSTICOS
# ------------------------------------------------------------

diag_ausentes <- mapa_colunas[
  coluna_encontrada == FALSE
]

diag_sem_valor <- disponibilidade[
  n_validos == 0
]

resumo_execucao <- data.table(
  indicador = c(
    "n_csv_totais",
    "n_arquivos_inmet",
    "n_arquivos_inmet_ok",
    "n_arquivos_inmet_erro",
    "n_estacoes_inmet_distintas",
    "n_estacoes_automaticas",
    "n_estacoes_convencionais",
    "n_estacao_ano_variavel",
    "n_colunas_alvo_ausentes",
    "n_estacao_ano_variavel_sem_valor"
  ),
  valor = c(
    length(
      arquivos
    ),
    length(
      arquivos_inmet
    ),
    sum(
      inventario_arquivos$status_leitura == "OK"
    ),
    sum(
      inventario_arquivos$status_leitura == "ERRO"
    ),
    uniqueN(
      inventario_arquivos[
        status_leitura == "OK",
        cod_estacao
      ]
    ),
    uniqueN(
      inventario_arquivos[
        status_leitura == "OK" &
          tipo_estacao == "AUTOMATICA",
        cod_estacao
      ]
    ),
    uniqueN(
      inventario_arquivos[
        status_leitura == "OK" &
          tipo_estacao == "CONVENCIONAL",
        cod_estacao
      ]
    ),
    nrow(
      disponibilidade
    ),
    nrow(
      diag_ausentes
    ),
    nrow(
      diag_sem_valor
    )
  )
)

# ------------------------------------------------------------
# 9. EXPORTAR
# ------------------------------------------------------------

fwrite(
  inventario_arquivos,
  file.path(
    PASTA_SAIDA,
    "inventario_arquivos_inmet_multivariaveis.csv"
  ),
  bom = TRUE
)

fwrite(
  mapa_colunas,
  file.path(
    PASTA_SAIDA,
    "mapa_colunas_inmet_multivariaveis.csv"
  ),
  bom = TRUE
)

fwrite(
  disponibilidade,
  file.path(
    PASTA_SAIDA,
    "disponibilidade_estacao_ano_variavel.csv"
  ),
  bom = TRUE
)

fwrite(
  resumo_estacoes,
  file.path(
    PASTA_SAIDA,
    "resumo_estacoes_inmet_multivariaveis.csv"
  ),
  bom = TRUE
)

fwrite(
  matriz,
  file.path(
    PASTA_SAIDA,
    "matriz_disponibilidade_percentual.csv"
  ),
  bom = TRUE
)

fwrite(
  diag_ausentes,
  file.path(
    PASTA_SAIDA,
    "diagnostico_colunas_alvo_ausentes.csv"
  ),
  bom = TRUE
)

fwrite(
  diag_sem_valor,
  file.path(
    PASTA_SAIDA,
    "diagnostico_variaveis_sem_valor.csv"
  ),
  bom = TRUE
)

fwrite(
  resumo_execucao,
  file.path(
    PASTA_SAIDA,
    "resumo_execucao_inventario_multivariavel.csv"
  ),
  bom = TRUE
)

# ------------------------------------------------------------
# 10. CONSOLE
# ------------------------------------------------------------

cat("\n============================================================\n")
cat("ETAPA 9I - INVENTARIO MULTIVARIAVEL INMET CONCLUIDO\n")
cat("============================================================\n\n")

print(
  resumo_execucao
)

cat("\nEstacoes inventariadas:\n")

print(
  resumo_estacoes[
    order(
      tipo_estacao,
      cod_estacao
    )
  ]
)

cat(
  "\nIMPORTANTE:\n",
  "- esta etapa apenas inventaria disponibilidade e estrutura;\n",
  "- temperatura, umidade, vento e pressao ainda nao foram consolidados em base analitica;\n",
  "- direcao do vento devera ser tratada como variavel circular nas etapas analiticas;\n",
  "- a proxima etapa deve fazer triagem espacial e temporal das estacoes automaticas.\n",
  sep = ""
)
