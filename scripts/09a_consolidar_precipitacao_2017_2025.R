# ============================================================
# ETAPA 9A - CONSOLIDACAO DOS DADOS DE PRECIPITACAO
# Fontes suportadas: CEMADEN e INMET
# Periodo analitico: 2017-2025
# Projeto de Mestrado - MP10 / Suape
# ============================================================
#
# OBJETIVO
# Ler automaticamente todos os CSVs da pasta "precipitacao",
# detectar o formato de cada arquivo, extrair a precipitacao e
# consolidar tudo em uma unica base padronizada e rastreavel.
#
# FORMATOS SUPORTADOS
# 1. CEMADEN:
#    municipio;codEstacao;uf;nomeEstacao;latitude;longitude;
#    datahora;valorMedida
#
# 2. INMET:
#    arquivos que iniciam com metadados como:
#    Nome:
#    Codigo Estacao:
#    Latitude:
#    Longitude:
#    ...
#    e depois possuem cabecalho iniciado por "Data Medicao;".
#
# PRINCIPIOS
# - Nenhum valor e excluido apenas pela magnitude.
# - Nenhuma lacuna e imputada.
# - Nenhuma agregacao horaria ou diaria e feita nesta etapa.
# - Duplicidades sao sinalizadas, nao resolvidas.
# - CEMADEN e INMET permanecem identificados pela fonte.
# - Diferencas de resolucao/periodicidade sao preservadas.
#
# FUSO HORARIO
# Confirme o fuso informado pelas fontes antes da analise final.
# Para os arquivos historicos utilizados neste projeto, o script
# parte da configuracao UTC e cria tambem o horario America/Recife.
# ============================================================

# ------------------------------------------------------------
# 0. PACOTE
# ------------------------------------------------------------

if (!requireNamespace("data.table", quietly = TRUE)) {
  stop(
    paste0(
      "O pacote 'data.table' nao esta instalado.\n",
      "Execute install.packages('data.table') e rode o script novamente."
    )
  )
}

library(data.table)

# ------------------------------------------------------------
# 1. CONFIGURACOES
# ------------------------------------------------------------

PASTA_ENTRADA <- "C:/Users/NIVEA/Documents/RStudio/Mestrado/precipitacao"

PASTA_SAIDA <- file.path(
  "C:/Users/NIVEA/Documents/RStudio/Mestrado",
  "outputs",
  "09_meteorologia",
  "precipitacao"
)

ANOS_ALVO <- 2017:2025

# Confirmar documentalmente antes da Etapa 11.
FUSO_CEMADEN <- "UTC"
FUSO_INMET <- "UTC"
FUSO_LOCAL <- "America/Recife"

# O RDS e o produto principal.
# CSV compactado pode ficar muito grande; deixe FALSE por padrao.
EXPORTAR_CSV_GZ <- FALSE

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

parse_datetime_flexivel <- function(x, tz) {
  x <- normalizar_texto(x)

  out <- as.POSIXct(
    rep(NA_character_, length(x)),
    tz = tz
  )

  formatos <- c(
    "%Y-%m-%d %H:%M:%OS",
    "%Y-%m-%d %H:%M",
    "%d/%m/%Y %H:%M:%OS",
    "%d/%m/%Y %H:%M",
    "%Y/%m/%d %H:%M:%OS",
    "%Y/%m/%d %H:%M"
  )

  for (fmt in formatos) {
    idx <- is.na(out) & !is.na(x)
    if (!any(idx)) break
    out[idx] <- as.POSIXct(
      x[idx],
      format = fmt,
      tz = tz
    )
  }

  out
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

ler_linhas_inicio <- function(arquivo, n = 30L) {
  # Tenta UTF-8 primeiro e usa latin1 como alternativa.
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

  sub("^\ufeff", "", z)
}

detectar_formato <- function(arquivo) {

  linhas <- ler_linhas_inicio(arquivo, n = 30L)

  if (length(linhas) == 0) {
    return("VAZIO_OU_ILEGIVEL")
  }

  chaves <- normalizar_chave(linhas)

  # INMET: bloco de metadados e cabecalho Data Medicao.
  if (
    any(grepl("^NOME_", chaves)) &&
    any(grepl("^CODIGO_ESTACAO_", chaves)) &&
    any(grepl("^DATA_MEDICAO_", chaves))
  ) {
    return("INMET")
  }

  # CEMADEN: primeira linha e cabecalho tabular direto.
  primeira_nao_vazia <- linhas[which(nzchar(trimws(linhas)))[1]]

  if (!is.na(primeira_nao_vazia)) {
    cab <- strsplit(primeira_nao_vazia, ";", fixed = TRUE)[[1]]
    cab_norm <- normalizar_chave(cab)

    if (
      all(
        c(
          "MUNICIPIO",
          "CODESTACAO",
          "UF",
          "NOMEESTACAO",
          "LATITUDE",
          "LONGITUDE",
          "DATAHORA",
          "VALORMEDIDA"
        ) %in% gsub("_", "", cab_norm)
      )
    ) {
      return("CEMADEN")
    }
  }

  "NAO_RECONHECIDO"
}

valor_metadado_inmet <- function(linhas, chave) {

  chaves_norm <- normalizar_chave(
    sub(":.*$", "", linhas)
  )

  alvo <- normalizar_chave(chave)
  idx <- which(chaves_norm == alvo)

  if (length(idx) == 0) return(NA_character_)

  normalizar_texto(
    sub("^[^:]+:\\s*", "", linhas[idx[1]])
  )
}

montar_datahora_inmet <- function(data_txt, hora_txt, tz) {

  datas <- parse_data_flexivel(data_txt)

  hora_txt <- normalizar_texto(hora_txt)
  hora_num <- suppressWarnings(
    as.integer(gsub("[^0-9]", "", hora_txt))
  )

  # Hora pode chegar como 0, 100, 1200 etc.
  hhmm <- ifelse(
    is.na(hora_num),
    NA_character_,
    sprintf("%04d", hora_num)
  )

  # Alguns sistemas historicos podem usar 2400.
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

  texto <- rep(NA_character_, length(datas))

  texto[valido] <- sprintf(
    "%s %02d:%02d:00",
    format(datas[valido], "%Y-%m-%d"),
    hh[valido],
    mm[valido]
  )

  as.POSIXct(
    texto,
    format = "%Y-%m-%d %H:%M:%S",
    tz = tz
  )
}

# ------------------------------------------------------------
# 3. LEITOR CEMADEN
# ------------------------------------------------------------

ler_cemaden <- function(arquivo) {

  x <- fread(
    arquivo,
    sep = ";",
    dec = ",",
    encoding = "UTF-8",
    na.strings = c("", "NA", "NULL", "null", "NaN"),
    showProgress = FALSE
  )

  setnames(x, names(x), sub("^\ufeff", "", names(x)))

  nomes_norm <- gsub("_", "", normalizar_chave(names(x)))

  mapa <- c(
    MUNICIPIO = "municipio",
    CODESTACAO = "cod_estacao",
    UF = "uf",
    NOMEESTACAO = "nome_estacao",
    LATITUDE = "latitude",
    LONGITUDE = "longitude",
    DATAHORA = "datahora_original",
    VALORMEDIDA = "precipitacao_mm"
  )

  idx <- match(names(mapa), nomes_norm)

  if (any(is.na(idx))) {
    stop(
      paste0(
        "Formato CEMADEN reconhecido, mas faltam colunas em ",
        basename(arquivo), ": ",
        paste(names(mapa)[is.na(idx)], collapse = ", ")
      )
    )
  }

  x <- x[, ..idx]
  setnames(x, unname(mapa))

  x[, municipio := toupper(normalizar_texto(municipio))]
  x[, cod_estacao := normalizar_texto(cod_estacao)]
  x[, uf := toupper(normalizar_texto(uf))]
  x[, nome_estacao := normalizar_texto(nome_estacao)]
  x[, latitude := para_numerico(latitude)]
  x[, longitude := para_numerico(longitude)]
  x[, precipitacao_mm := para_numerico(precipitacao_mm)]
  x[, datahora_original := normalizar_texto(datahora_original)]

  x[, datahora_origem := parse_datetime_flexivel(
    datahora_original,
    tz = FUSO_CEMADEN
  )]

  x[, `:=`(
    fonte_dados = "CEMADEN",
    tipo_estacao = "PLUVIOMETRICA_AUTOMATICA",
    altitude_m = NA_real_,
    situacao_estacao = NA_character_,
    periodicidade_fonte = NA_character_,
    data_inicial_metadado = as.Date(NA),
    data_final_metadado = as.Date(NA),
    arquivo_origem = basename(arquivo),
    formato_origem = "CEMADEN"
  )]

  x
}

# ------------------------------------------------------------
# 4. LEITOR INMET
# ------------------------------------------------------------

ler_inmet <- function(arquivo) {

  linhas <- ler_linhas_inicio(arquivo, n = 60L)

  linhas_norm <- normalizar_chave(linhas)

  idx_header <- which(grepl("^DATA_MEDICAO_", linhas_norm))[1]

  if (is.na(idx_header)) {
    stop(
      paste0(
        "Cabecalho 'Data Medicao' nao encontrado em ",
        basename(arquivo)
      )
    )
  }

  nome_estacao <- valor_metadado_inmet(linhas, "Nome")
  cod_estacao <- valor_metadado_inmet(linhas, "Codigo Estacao")
  latitude <- para_numerico(
    valor_metadado_inmet(linhas, "Latitude")
  )
  longitude <- para_numerico(
    valor_metadado_inmet(linhas, "Longitude")
  )
  altitude <- para_numerico(
    valor_metadado_inmet(linhas, "Altitude")
  )
  situacao <- valor_metadado_inmet(linhas, "Situacao")
  data_inicial <- parse_data_flexivel(
    valor_metadado_inmet(linhas, "Data Inicial")
  )
  data_final <- parse_data_flexivel(
    valor_metadado_inmet(linhas, "Data Final")
  )
  periodicidade <- valor_metadado_inmet(
    linhas,
    "Periodicidade da Medicao"
  )

  raw <- fread(
    arquivo,
    skip = idx_header - 1L,
    sep = ";",
    dec = ",",
    encoding = "UTF-8",
    na.strings = c("", "NA", "NULL", "null", "NaN"),
    fill = TRUE,
    showProgress = FALSE
  )

  setnames(
    raw,
    names(raw),
    sub("^\ufeff", "", names(raw))
  )

  nomes_norm <- normalizar_chave(names(raw))

  idx_data <- which(
    nomes_norm %in% c(
      "DATA_MEDICAO",
      "DATA"
    )
  )[1]

  idx_hora <- which(
    nomes_norm %in% c(
      "HORA_MEDICAO",
      "HORA_UTC",
      "HORA"
    )
  )[1]

  idx_prec <- which(
    grepl("PRECIPITACAO", nomes_norm) &
      grepl("TOTAL", nomes_norm) &
      grepl("HORARIO", nomes_norm)
  )[1]

  # Fallback: qualquer coluna de precipitacao em mm.
  if (is.na(idx_prec)) {
    idx_prec <- which(
      grepl("PRECIPITACAO", nomes_norm) &
        grepl("MM", nomes_norm)
    )[1]
  }

  faltantes <- character()

  if (is.na(idx_data)) faltantes <- c(faltantes, "Data Medicao")
  if (is.na(idx_hora)) faltantes <- c(faltantes, "Hora Medicao")
  if (is.na(idx_prec)) faltantes <- c(faltantes, "Precipitacao")

  if (length(faltantes) > 0) {
    stop(
      paste0(
        "INMET reconhecido, mas nao foi possivel localizar: ",
        paste(faltantes, collapse = ", "),
        " em ", basename(arquivo)
      )
    )
  }

  data_txt <- as.character(raw[[idx_data]])
  hora_txt <- as.character(raw[[idx_hora]])
  precip <- para_numerico(raw[[idx_prec]])

  dh <- montar_datahora_inmet(
    data_txt,
    hora_txt,
    tz = FUSO_INMET
  )

  # Convencional costuma ter codigo numerico; automatica costuma
  # utilizar A###. A classificacao e mantida explicita e auditavel.
  tipo_estacao <- if (
    !is.na(cod_estacao) &&
    grepl("^A[0-9]+$", toupper(cod_estacao))
  ) {
    "AUTOMATICA"
  } else {
    "CONVENCIONAL"
  }

  data.table(
    municipio = NA_character_,
    cod_estacao = normalizar_texto(cod_estacao),
    uf = NA_character_,
    nome_estacao = normalizar_texto(nome_estacao),
    latitude = latitude,
    longitude = longitude,
    datahora_original = paste(
      normalizar_texto(data_txt),
      normalizar_texto(hora_txt)
    ),
    precipitacao_mm = precip,
    datahora_origem = dh,
    fonte_dados = "INMET",
    tipo_estacao = tipo_estacao,
    altitude_m = altitude,
    situacao_estacao = normalizar_texto(situacao),
    periodicidade_fonte = normalizar_texto(periodicidade),
    data_inicial_metadado = data_inicial,
    data_final_metadado = data_final,
    arquivo_origem = basename(arquivo),
    formato_origem = "INMET"
  )
}

# ------------------------------------------------------------
# 5. LOCALIZAR ARQUIVOS
# ------------------------------------------------------------

arquivos <- list.files(
  PASTA_ENTRADA,
  pattern = "\\.csv$",
  full.names = TRUE,
  recursive = FALSE,
  ignore.case = TRUE
)

if (length(arquivos) == 0) {
  stop(
    paste0(
      "Nenhum CSV encontrado em: ",
      PASTA_ENTRADA,
      "\nConfirme o nome e a localizacao da pasta."
    )
  )
}

cat("\nArquivos CSV encontrados:", length(arquivos), "\n\n")

# ------------------------------------------------------------
# 6. DETECTAR, LER E AUDITAR CADA ARQUIVO
# ------------------------------------------------------------

lista_dados <- list()
lista_diag <- vector("list", length(arquivos))

n_ok <- 0L

for (i in seq_along(arquivos)) {

  arquivo <- arquivos[i]
  formato <- detectar_formato(arquivo)

  cat(
    sprintf(
      "[%03d/%03d] %-8s %s\n",
      i,
      length(arquivos),
      formato,
      basename(arquivo)
    )
  )

  resultado <- tryCatch(
    {

      if (formato == "CEMADEN") {
        x <- ler_cemaden(arquivo)
      } else if (formato == "INMET") {
        x <- ler_inmet(arquivo)
      } else {
        stop(
          paste0(
            "Formato nao reconhecido: ",
            formato
          )
        )
      }

      n_ok <- n_ok + 1L
      lista_dados[[n_ok]] <- x

      data.table(
        arquivo = basename(arquivo),
        formato_detectado = formato,
        status_leitura = "OK",
        mensagem = NA_character_,
        n_registros = nrow(x),
        fonte_dados = paste(
          unique(na.omit(x$fonte_dados)),
          collapse = " | "
        ),
        cod_estacoes = paste(
          unique(na.omit(x$cod_estacao)),
          collapse = " | "
        ),
        nome_estacoes = paste(
          unique(na.omit(x$nome_estacao)),
          collapse = " | "
        ),
        inicio = if (all(is.na(x$datahora_origem))) {
          as.POSIXct(NA)
        } else {
          min(x$datahora_origem, na.rm = TRUE)
        },
        fim = if (all(is.na(x$datahora_origem))) {
          as.POSIXct(NA)
        } else {
          max(x$datahora_origem, na.rm = TRUE)
        },
        n_datahora_na = sum(is.na(x$datahora_origem)),
        n_precipitacao_na = sum(is.na(x$precipitacao_mm)),
        n_precipitacao_negativa = sum(
          x$precipitacao_mm < 0,
          na.rm = TRUE
        )
      )
    },
    error = function(e) {

      data.table(
        arquivo = basename(arquivo),
        formato_detectado = formato,
        status_leitura = "ERRO",
        mensagem = conditionMessage(e),
        n_registros = NA_integer_,
        fonte_dados = NA_character_,
        cod_estacoes = NA_character_,
        nome_estacoes = NA_character_,
        inicio = as.POSIXct(NA),
        fim = as.POSIXct(NA),
        n_datahora_na = NA_integer_,
        n_precipitacao_na = NA_integer_,
        n_precipitacao_negativa = NA_integer_
      )
    }
  )

  lista_diag[[i]] <- resultado
}

diagnostico_arquivos <- rbindlist(
  lista_diag,
  fill = TRUE
)

fwrite(
  diagnostico_arquivos,
  file.path(
    PASTA_SAIDA,
    "diagnostico_leitura_arquivos_precipitacao.csv"
  ),
  bom = TRUE
)

arquivos_com_erro <- diagnostico_arquivos[
  status_leitura != "OK"
]

fwrite(
  arquivos_com_erro,
  file.path(
    PASTA_SAIDA,
    "arquivos_nao_lidos_precipitacao.csv"
  ),
  bom = TRUE
)

if (length(lista_dados) == 0) {
  stop(
    paste0(
      "Nenhum arquivo foi lido com sucesso.\n",
      "Consulte: ",
      file.path(
        PASTA_SAIDA,
        "diagnostico_leitura_arquivos_precipitacao.csv"
      )
    )
  )
}

# ------------------------------------------------------------
# 7. CONSOLIDAR AS FONTES
# ------------------------------------------------------------

precipitacao <- rbindlist(
  lista_dados,
  use.names = TRUE,
  fill = TRUE
)

rm(lista_dados)
gc()

precipitacao[
  ,
  id_estacao_fonte := paste(
    fonte_dados,
    cod_estacao,
    sep = "::"
  )
]

# Cria representacoes UTC e local.
precipitacao[
  fonte_dados == "CEMADEN",
  datahora_utc := converter_fuso(
    datahora_origem,
    "UTC"
  )
]

precipitacao[
  fonte_dados == "INMET",
  datahora_utc := converter_fuso(
    datahora_origem,
    "UTC"
  )
]

precipitacao[
  ,
  datahora_local := converter_fuso(
    datahora_utc,
    FUSO_LOCAL
  )
]

precipitacao[, data_local := as.IDate(datahora_local)]
precipitacao[, ano := as.integer(format(datahora_local, "%Y"))]
precipitacao[, mes := as.integer(format(datahora_local, "%m"))]
precipitacao[, dia := as.integer(format(datahora_local, "%d"))]
precipitacao[, hora := as.integer(format(datahora_local, "%H"))]
precipitacao[, minuto := as.integer(format(datahora_local, "%M"))]

# Flags basicas.
precipitacao[
  ,
  flag_datahora_ausente := is.na(datahora_local)
]

precipitacao[
  ,
  flag_precipitacao_ausente := is.na(precipitacao_mm)
]

precipitacao[
  ,
  flag_precipitacao_negativa := (
    !is.na(precipitacao_mm) &
      precipitacao_mm < 0
  )
]

# Mantem apenas 2017-2025 quando o ano puder ser determinado.
n_fora_periodo <- precipitacao[
  !is.na(ano) & !ano %in% ANOS_ALVO,
  .N
]

precipitacao <- precipitacao[
  is.na(ano) | ano %in% ANOS_ALVO
]

precipitacao[, id_registro := .I]

# ------------------------------------------------------------
# 8. DUPLICIDADES DENTRO DA MESMA FONTE/ESTACAO
# ------------------------------------------------------------

precipitacao[
  ,
  n_registros_estacao_tempo := .N,
  by = .(
    fonte_dados,
    cod_estacao,
    datahora_local
  )
]

precipitacao[
  ,
  n_valores_estacao_tempo := uniqueN(
    precipitacao_mm,
    na.rm = FALSE
  ),
  by = .(
    fonte_dados,
    cod_estacao,
    datahora_local
  )
]

precipitacao[
  ,
  flag_duplicado_estacao_tempo :=
    n_registros_estacao_tempo > 1
]

precipitacao[
  ,
  flag_conflito_estacao_tempo := (
    n_registros_estacao_tempo > 1 &
      n_valores_estacao_tempo > 1
  )
]

duplicidades <- precipitacao[
  flag_duplicado_estacao_tempo == TRUE,
  .(
    nome_estacao = paste(
      sort(unique(na.omit(nome_estacao))),
      collapse = " | "
    ),
    n_registros = .N,
    n_valores_distintos = uniqueN(
      precipitacao_mm,
      na.rm = FALSE
    ),
    valores_mm = paste(
      sort(unique(precipitacao_mm)),
      collapse = " | "
    ),
    arquivos = paste(
      sort(unique(arquivo_origem)),
      collapse = " | "
    )
  ),
  by = .(
    fonte_dados,
    cod_estacao,
    datahora_local
  )
][
  order(
    fonte_dados,
    cod_estacao,
    datahora_local
  )
]

fwrite(
  duplicidades,
  file.path(
    PASTA_SAIDA,
    "diagnostico_duplicidades_precipitacao.csv"
  ),
  bom = TRUE
)

# ------------------------------------------------------------
# 9. INVENTARIO DE ESTACOES
# ------------------------------------------------------------

inventario_estacoes <- precipitacao[
  ,
  .(
    nome_estacao = paste(
      sort(unique(na.omit(nome_estacao))),
      collapse = " | "
    ),
    municipios = paste(
      sort(unique(na.omit(municipio))),
      collapse = " | "
    ),
    uf = paste(
      sort(unique(na.omit(uf))),
      collapse = " | "
    ),
    tipo_estacao = paste(
      sort(unique(na.omit(tipo_estacao))),
      collapse = " | "
    ),
    periodicidade_fonte = paste(
      sort(unique(na.omit(periodicidade_fonte))),
      collapse = " | "
    ),
    situacao_estacao = paste(
      sort(unique(na.omit(situacao_estacao))),
      collapse = " | "
    ),
    altitude_m = {
      z <- unique(altitude_m[is.finite(altitude_m)])
      if (length(z) == 0) NA_real_ else z[1]
    },
    latitude_min = {
      z <- latitude[is.finite(latitude)]
      if (length(z) == 0) NA_real_ else min(z)
    },
    latitude_max = {
      z <- latitude[is.finite(latitude)]
      if (length(z) == 0) NA_real_ else max(z)
    },
    longitude_min = {
      z <- longitude[is.finite(longitude)]
      if (length(z) == 0) NA_real_ else min(z)
    },
    longitude_max = {
      z <- longitude[is.finite(longitude)]
      if (length(z) == 0) NA_real_ else max(z)
    },
    n_registros = .N,
    inicio_local = if (all(is.na(datahora_local))) {
      as.POSIXct(NA)
    } else {
      min(datahora_local, na.rm = TRUE)
    },
    fim_local = if (all(is.na(datahora_local))) {
      as.POSIXct(NA)
    } else {
      max(datahora_local, na.rm = TRUE)
    },
    anos = paste(
      sort(unique(na.omit(ano))),
      collapse = ";"
    ),
    n_precipitacao_ausente =
      sum(flag_precipitacao_ausente),
    n_precipitacao_negativa =
      sum(flag_precipitacao_negativa),
    n_registros_duplicados =
      sum(flag_duplicado_estacao_tempo),
    n_registros_conflitantes =
      sum(flag_conflito_estacao_tempo)
  ),
  by = .(
    fonte_dados,
    cod_estacao
  )
][
  order(
    fonte_dados,
    cod_estacao
  )
]

fwrite(
  inventario_estacoes,
  file.path(
    PASTA_SAIDA,
    "inventario_estacoes_precipitacao.csv"
  ),
  bom = TRUE
)

# ------------------------------------------------------------
# 10. COBERTURA POR FONTE, ESTACAO E ANO
# ------------------------------------------------------------

cobertura_estacao_ano <- precipitacao[
  !is.na(ano),
  .(
    n_registros = .N,
    n_registros_com_valor =
      sum(!is.na(precipitacao_mm)),
    n_registros_sem_valor =
      sum(is.na(precipitacao_mm)),
    n_datas_com_registro =
      uniqueN(data_local, na.rm = TRUE),
    n_meses_com_registro =
      uniqueN(mes, na.rm = TRUE),
    inicio = if (all(is.na(datahora_local))) {
      as.POSIXct(NA)
    } else {
      min(datahora_local, na.rm = TRUE)
    },
    fim = if (all(is.na(datahora_local))) {
      as.POSIXct(NA)
    } else {
      max(datahora_local, na.rm = TRUE)
    },
    precipitacao_min = if (
      all(is.na(precipitacao_mm))
    ) {
      NA_real_
    } else {
      min(precipitacao_mm, na.rm = TRUE)
    },
    precipitacao_max = if (
      all(is.na(precipitacao_mm))
    ) {
      NA_real_
    } else {
      max(precipitacao_mm, na.rm = TRUE)
    }
  ),
  by = .(
    fonte_dados,
    cod_estacao,
    nome_estacao,
    ano
  )
][
  order(
    fonte_dados,
    cod_estacao,
    ano
  )
]

fwrite(
  cobertura_estacao_ano,
  file.path(
    PASTA_SAIDA,
    "cobertura_precipitacao_estacao_ano.csv"
  ),
  bom = TRUE
)

# ------------------------------------------------------------
# 11. COBERTURA MENSAL
# ------------------------------------------------------------

cobertura_mensal <- precipitacao[
  !is.na(ano) & !is.na(mes),
  .(
    n_registros = .N,
    n_registros_com_valor =
      sum(!is.na(precipitacao_mm)),
    n_registros_sem_valor =
      sum(is.na(precipitacao_mm)),
    n_estacoes = uniqueN(id_estacao_fonte),
    inicio = min(datahora_local, na.rm = TRUE),
    fim = max(datahora_local, na.rm = TRUE)
  ),
  by = .(
    fonte_dados,
    ano,
    mes
  )
][
  order(
    fonte_dados,
    ano,
    mes
  )
]

fwrite(
  cobertura_mensal,
  file.path(
    PASTA_SAIDA,
    "cobertura_mensal_precipitacao.csv"
  ),
  bom = TRUE
)

# ------------------------------------------------------------
# 12. MESES PRESENTES POR FONTE
# ------------------------------------------------------------

fontes_presentes <- sort(
  unique(na.omit(precipitacao$fonte_dados))
)

grade_meses <- CJ(
  fonte_dados = fontes_presentes,
  ano = ANOS_ALVO,
  mes = 1:12,
  unique = TRUE
)

diagnostico_meses <- merge(
  grade_meses,
  cobertura_mensal[
    ,
    .(
      fonte_dados,
      ano,
      mes,
      n_registros,
      n_estacoes
    )
  ],
  by = c(
    "fonte_dados",
    "ano",
    "mes"
  ),
  all.x = TRUE
)

diagnostico_meses[
  ,
  mes_presente :=
    !is.na(n_registros) &
    n_registros > 0
]

fwrite(
  diagnostico_meses,
  file.path(
    PASTA_SAIDA,
    "diagnostico_meses_2017_2025.csv"
  ),
  bom = TRUE
)

# ------------------------------------------------------------
# 13. RESUMO POR FONTE
# ------------------------------------------------------------

resumo_fonte <- precipitacao[
  ,
  .(
    n_registros = .N,
    n_estacoes = uniqueN(
      cod_estacao,
      na.rm = TRUE
    ),
    n_anos = uniqueN(
      ano,
      na.rm = TRUE
    ),
    n_precipitacao_ausente =
      sum(flag_precipitacao_ausente),
    n_precipitacao_negativa =
      sum(flag_precipitacao_negativa),
    n_registros_duplicados =
      sum(flag_duplicado_estacao_tempo),
    n_registros_conflitantes =
      sum(flag_conflito_estacao_tempo)
  ),
  by = fonte_dados
][
  order(fonte_dados)
]

fwrite(
  resumo_fonte,
  file.path(
    PASTA_SAIDA,
    "resumo_por_fonte_precipitacao.csv"
  ),
  bom = TRUE
)

# ------------------------------------------------------------
# 14. ORDENAR E SALVAR BASE CONSOLIDADA
# ------------------------------------------------------------

setorder(
  precipitacao,
  fonte_dados,
  cod_estacao,
  datahora_local,
  arquivo_origem
)

setcolorder(
  precipitacao,
  c(
    "id_registro",
    "fonte_dados",
    "formato_origem",
    "tipo_estacao",
    "cod_estacao",
    "id_estacao_fonte",
    "nome_estacao",
    "municipio",
    "uf",
    "latitude",
    "longitude",
    "altitude_m",
    "situacao_estacao",
    "periodicidade_fonte",
    "data_inicial_metadado",
    "data_final_metadado",
    "datahora_original",
    "datahora_origem",
    "datahora_utc",
    "datahora_local",
    "data_local",
    "ano",
    "mes",
    "dia",
    "hora",
    "minuto",
    "precipitacao_mm",
    "arquivo_origem",
    "flag_datahora_ausente",
    "flag_precipitacao_ausente",
    "flag_precipitacao_negativa",
    "n_registros_estacao_tempo",
    "n_valores_estacao_tempo",
    "flag_duplicado_estacao_tempo",
    "flag_conflito_estacao_tempo"
  )
)

arquivo_rds <- file.path(
  PASTA_SAIDA,
  "precipitacao_2017_2025_consolidada.rds"
)

saveRDS(
  precipitacao,
  arquivo_rds,
  compress = "gzip"
)

if (EXPORTAR_CSV_GZ) {

  arquivo_csv_gz <- file.path(
    PASTA_SAIDA,
    "precipitacao_2017_2025_consolidada.csv.gz"
  )

  fwrite(
    precipitacao,
    arquivo_csv_gz,
    sep = ";",
    dec = ",",
    bom = TRUE,
    compress = "auto"
  )
}

# ------------------------------------------------------------
# 15. RESUMO FINAL
# ------------------------------------------------------------

resumo_execucao <- data.table(
  indicador = c(
    "n_arquivos_encontrados",
    "n_arquivos_lidos_ok",
    "n_arquivos_com_erro",
    "n_registros_consolidados",
    "n_fontes",
    "n_estacoes_fonte",
    "n_registros_datahora_ausente",
    "n_registros_precipitacao_ausente",
    "n_registros_precipitacao_negativa",
    "n_registros_duplicados_estacao_tempo",
    "n_registros_conflitantes_estacao_tempo",
    "n_registros_fora_periodo_local_removidos"
  ),
  valor = c(
    length(arquivos),
    sum(diagnostico_arquivos$status_leitura == "OK"),
    sum(diagnostico_arquivos$status_leitura != "OK"),
    nrow(precipitacao),
    uniqueN(precipitacao$fonte_dados),
    uniqueN(precipitacao$id_estacao_fonte),
    sum(precipitacao$flag_datahora_ausente),
    sum(precipitacao$flag_precipitacao_ausente),
    sum(precipitacao$flag_precipitacao_negativa),
    sum(precipitacao$flag_duplicado_estacao_tempo),
    sum(precipitacao$flag_conflito_estacao_tempo),
    n_fora_periodo
  )
)

fwrite(
  resumo_execucao,
  file.path(
    PASTA_SAIDA,
    "resumo_execucao_consolidacao_precipitacao.csv"
  ),
  bom = TRUE
)

# ------------------------------------------------------------
# 16. RELATORIO NO CONSOLE
# ------------------------------------------------------------

cat("\n============================================================\n")
cat("ETAPA 9A - CONSOLIDACAO DE PRECIPITACAO CONCLUIDA\n")
cat("============================================================\n\n")

print(resumo_execucao)

cat("\nResumo por fonte:\n")
print(resumo_fonte)

if (nrow(arquivos_com_erro) > 0) {

  cat(
    "\nATENCAO: alguns arquivos nao foram lidos.\n",
    "Consulte:\n",
    file.path(
      PASTA_SAIDA,
      "arquivos_nao_lidos_precipitacao.csv"
    ),
    "\n",
    sep = ""
  )

} else {

  cat("\nTodos os arquivos foram lidos com sucesso.\n")
}

cat("\nTabela principal:\n")
cat(arquivo_rds, "\n")

if (EXPORTAR_CSV_GZ) {
  cat("\nCSV compactado:\n")
  cat(arquivo_csv_gz, "\n")
}

cat("\nDiagnosticos:\n")
cat(PASTA_SAIDA, "\n")

cat(
  "\nIMPORTANTE:\n",
  "- nenhuma chuva foi agregada por hora/dia;\n",
  "- CEMADEN e INMET permanecem identificados;\n",
  "- as periodicidades originais foram preservadas;\n",
  "- confirme documentalmente os fusos antes da integracao final;\n",
  "- o proximo passo e avaliar cobertura e selecionar estacoes.\n",
  sep = ""
)
