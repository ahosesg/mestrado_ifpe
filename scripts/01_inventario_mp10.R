# ============================================================
# ETAPA 1 - INVENTARIO DEFINITIVO DA BASE HISTORICA DE MP10
# Pernambuco, 2017-2025
# ============================================================
#
# Objetivo:
# Produzir um inventario reproduzivel da disponibilidade bruta
# de MP10 por estacao e ano antes da aplicacao dos criterios
# formais de controle de qualidade e completude.
#
# IMPORTANTE:
# - Este script NAO define ainda quais registros sao validos.
# - As medidas de cobertura calculadas aqui sao diagnosticas e
#   nao devem ser tratadas como criterios finais de completude.
# - A base v1 e preservada sem alteracoes.
# - O caso de 2023 recebe diagnostico especifico quando o arquivo
#   bruto Dados_MonitorAr_2023.csv esta disponivel localmente.
# ============================================================

library(dplyr)

# ------------------------------------------------------------
# 0. CONFIGURACAO
# ------------------------------------------------------------

arquivo_base <- "dados_MP10_2017_2025_PE_v1.rds"
pasta_saida <- file.path("outputs", "01_inventario")

dir.create(pasta_saida, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(arquivo_base)) {
  stop(
    paste0(
      "Arquivo nao encontrado: ", arquivo_base,
      ". Execute o script a partir da raiz do projeto Mestrado.Rproj."
    )
  )
}

# Funcoes auxiliares -------------------------------------------------------

colapsar_unicos <- function(x) {
  x <- unique(na.omit(as.character(x)))
  x <- x[nzchar(x)]
  if (length(x) == 0) return(NA_character_)
  paste(sort(x), collapse = " | ")
}

min_data_segura <- function(x) {
  if (all(is.na(x))) return(as.Date(NA))
  min(as.Date(x), na.rm = TRUE)
}

max_data_segura <- function(x) {
  if (all(is.na(x))) return(as.Date(NA))
  max(as.Date(x), na.rm = TRUE)
}

mediana_segura <- function(x) {
  if (all(is.na(x))) return(NA_real_)
  median(as.numeric(x), na.rm = TRUE)
}

min_num_seguro <- function(x) {
  if (all(is.na(x))) return(NA_real_)
  min(as.numeric(x), na.rm = TRUE)
}

max_num_seguro <- function(x) {
  if (all(is.na(x))) return(NA_real_)
  max(as.numeric(x), na.rm = TRUE)
}

ano_bissexto <- function(ano) {
  (ano %% 400 == 0) | ((ano %% 4 == 0) & (ano %% 100 != 0))
}

arquivo_origem_por_ano <- function(ano) {
  ifelse(
    ano <= 2022,
    paste0("PE", ano, ".csv"),
    paste0("Dados_MonitorAr_", ano, ".csv")
  )
}

# ------------------------------------------------------------
# 1. CARREGAR A BASE CONSOLIDADA V1
# ------------------------------------------------------------

dados <- readRDS(arquivo_base)

colunas_necessarias <- c(
  "ano", "cod_estacao", "no_estacao", "nu_concentracao",
  "data_fechada", "hora_fechada", "nu_latitude", "nu_longitude"
)

faltantes <- setdiff(colunas_necessarias, names(dados))
if (length(faltantes) > 0) {
  stop(
    paste(
      "A base v1 nao contem as colunas necessarias:",
      paste(faltantes, collapse = ", ")
    )
  )
}

# Garante classes basicas sem alterar o arquivo de origem.
dados <- dados %>%
  mutate(
    ano = as.integer(ano),
    data_fechada = as.Date(data_fechada),
    nu_concentracao = suppressWarnings(as.numeric(nu_concentracao)),
    cod_estacao = as.character(cod_estacao),
    no_estacao = as.character(no_estacao)
  )

# ------------------------------------------------------------
# 2. INVENTARIO OBSERVADO POR ESTACAO E ANO
# ------------------------------------------------------------

# As colunas opcionais sao usadas somente quando existirem na v1.
if (!"no_fonte_dados" %in% names(dados)) dados$no_fonte_dados <- NA_character_
if (!"st_estacao" %in% names(dados)) dados$st_estacao <- NA_character_
if (!"no_municipio" %in% names(dados)) dados$no_municipio <- NA_character_
if (!"origem_coordenada" %in% names(dados)) dados$origem_coordenada <- NA_character_

inventario_observado <- dados %>%
  group_by(ano, cod_estacao, no_estacao) %>%
  summarise(
    n_registros = n(),
    n_concentracao_informada = sum(!is.na(nu_concentracao)),
    n_concentracao_ausente = sum(is.na(nu_concentracao)),
    n_dias_com_registro = n_distinct(data_fechada[!is.na(data_fechada)]),
    n_meses_com_registro = n_distinct(format(data_fechada[!is.na(data_fechada)], "%Y-%m")),
    n_horas_distintas = n_distinct(hora_fechada[!is.na(hora_fechada)]),
    primeira_medicao = min_data_segura(data_fechada),
    ultima_medicao = max_data_segura(data_fechada),
    latitude_mediana = mediana_segura(nu_latitude),
    longitude_mediana = mediana_segura(nu_longitude),
    latitude_min = min_num_seguro(nu_latitude),
    latitude_max = max_num_seguro(nu_latitude),
    longitude_min = min_num_seguro(nu_longitude),
    longitude_max = max_num_seguro(nu_longitude),
    n_pares_coordenadas = n_distinct(
      paste(
        round(nu_latitude[!is.na(nu_latitude) & !is.na(nu_longitude)], 6),
        round(nu_longitude[!is.na(nu_latitude) & !is.na(nu_longitude)], 6),
        sep = ";"
      )
    ),
    origem_coordenada = colapsar_unicos(origem_coordenada),
    fonte_registro = colapsar_unicos(no_fonte_dados),
    situacao_estacao = colapsar_unicos(st_estacao),
    municipio = colapsar_unicos(no_municipio),
    .groups = "drop"
  ) %>%
  mutate(
    dias_teoricos_ano = ifelse(ano_bissexto(ano), 366L, 365L),
    horas_teoricas_ano = dias_teoricos_ano * 24L,
    cobertura_bruta_dias_pct = 100 * n_dias_com_registro / dias_teoricos_ano,
    cobertura_bruta_registros_pct = 100 * n_registros / horas_teoricas_ano,
    arquivo_origem = arquivo_origem_por_ano(ano)
  )

# ------------------------------------------------------------
# 3. GRADE COMPLETA ESTACAO x ANO
# ------------------------------------------------------------

# A grade explicita os anos sem qualquer registro. Isso e
# importante para nao confundir ausencia na tabela com zero.

estacoes <- dados %>%
  distinct(cod_estacao, no_estacao) %>%
  arrange(cod_estacao, no_estacao)

grade <- merge(
  data.frame(ano = 2017:2025),
  estacoes,
  by = NULL
)

inventario_estacao_ano <- grade %>%
  left_join(
    inventario_observado,
    by = c("ano", "cod_estacao", "no_estacao")
  ) %>%
  mutate(
    arquivo_origem = ifelse(
      is.na(arquivo_origem),
      arquivo_origem_por_ano(ano),
      arquivo_origem
    ),
    possui_registro = !is.na(n_registros),
    n_registros = ifelse(is.na(n_registros), 0L, n_registros),
    n_concentracao_informada = ifelse(
      is.na(n_concentracao_informada), 0L, n_concentracao_informada
    ),
    n_concentracao_ausente = ifelse(
      is.na(n_concentracao_ausente), 0L, n_concentracao_ausente
    ),
    n_dias_com_registro = ifelse(
      is.na(n_dias_com_registro), 0L, n_dias_com_registro
    ),
    n_meses_com_registro = ifelse(
      is.na(n_meses_com_registro), 0L, n_meses_com_registro
    ),
    n_horas_distintas = ifelse(
      is.na(n_horas_distintas), 0L, n_horas_distintas
    ),
    dias_teoricos_ano = ifelse(ano_bissexto(ano), 366L, 365L),
    horas_teoricas_ano = dias_teoricos_ano * 24L,
    cobertura_bruta_dias_pct = 100 * n_dias_com_registro / dias_teoricos_ano,
    cobertura_bruta_registros_pct = 100 * n_registros / horas_teoricas_ano
  ) %>%
  arrange(ano, cod_estacao, no_estacao)

# ------------------------------------------------------------
# 4. INVENTARIO CONSOLIDADO POR ESTACAO
# ------------------------------------------------------------

inventario_estacoes <- dados %>%
  group_by(cod_estacao, no_estacao) %>%
  summarise(
    primeiro_ano_com_registro = min(ano, na.rm = TRUE),
    ultimo_ano_com_registro = max(ano, na.rm = TRUE),
    anos_com_registro = paste(sort(unique(ano)), collapse = ";"),
    n_anos_com_registro = n_distinct(ano),
    n_registros_total = n(),
    primeira_medicao = min_data_segura(data_fechada),
    ultima_medicao = max_data_segura(data_fechada),
    latitude_referencia = mediana_segura(nu_latitude),
    longitude_referencia = mediana_segura(nu_longitude),
    latitude_min = min_num_seguro(nu_latitude),
    latitude_max = max_num_seguro(nu_latitude),
    longitude_min = min_num_seguro(nu_longitude),
    longitude_max = max_num_seguro(nu_longitude),
    municipio = colapsar_unicos(no_municipio),
    fonte_registro = colapsar_unicos(no_fonte_dados),
    situacao_estacao = colapsar_unicos(st_estacao),
    origem_coordenada = colapsar_unicos(origem_coordenada),
    .groups = "drop"
  ) %>%
  arrange(cod_estacao, no_estacao)

# ------------------------------------------------------------
# 5. METADADOS DISPONIVEIS NAS BASES PE2017-PE2022
# ------------------------------------------------------------

arquivos_antigos <- paste0("PE", 2017:2022, ".csv")

ler_metadados_antigos <- function(arq) {
  if (!file.exists(arq)) return(NULL)
  ano_ref <- as.integer(gsub("[^0-9]", "", arq))
  x <- read.csv(arq, stringsAsFactors = FALSE)

  colunas_esperadas <- c("Estacao", "Codigo", "Poluente", "Unidade", "Tipo")
  if (!all(colunas_esperadas %in% names(x))) {
    warning(paste("Estrutura inesperada em", arq))
    return(NULL)
  }

  x %>%
    filter(Poluente == "MP10") %>%
    group_by(Estacao, Codigo, Unidade, Tipo) %>%
    summarise(n_registros_origem = n(), .groups = "drop") %>%
    mutate(
      ano = ano_ref,
      arquivo_origem = arq,
      .before = 1
    )
}

metadados_2017_2022 <- bind_rows(lapply(arquivos_antigos, ler_metadados_antigos))

# ------------------------------------------------------------
# 6. ESQUEMA DOS ARQUIVOS MONITORAR 2023-2025
# ------------------------------------------------------------

# Ler apenas a primeira linha permite identificar quais campos
# existem nos arquivos brutos sem carregar centenas de MB.

arquivos_monitorar <- paste0("Dados_MonitorAr_", 2023:2025, ".csv")

esquema_monitorar <- bind_rows(lapply(arquivos_monitorar, function(arq) {
  if (!file.exists(arq)) {
    return(data.frame(
      arquivo = arq,
      disponivel_localmente = FALSE,
      coluna = NA_character_
    ))
  }

  cab <- read.csv(
    arq,
    nrows = 1,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  data.frame(
    arquivo = arq,
    disponivel_localmente = TRUE,
    coluna = names(cab)
  )
}))

# ------------------------------------------------------------
# 7. DIAGNOSTICO ESPECIFICO DO MONITORAR 2023
# ------------------------------------------------------------

# O processamento anterior encontrou zero registros para
# sg_uf == "PE" e cd_normalizado == "MP10" em 2023.
# O bloco abaixo verifica se isso decorre de ausencia de PE,
# ausencia de MP10 em PE ou possivel codificacao diferente.
#
# Para evitar carregar centenas de MB com read.csv(), utiliza-se
# data.table::fread quando o pacote estiver instalado.

arquivo_2023 <- "Dados_MonitorAr_2023.csv"

diag_2023_status <- data.frame(
  arquivo = arquivo_2023,
  arquivo_disponivel = file.exists(arquivo_2023),
  data_table_disponivel = requireNamespace("data.table", quietly = TRUE),
  diagnostico_executado = FALSE,
  observacao = NA_character_
)

if (file.exists(arquivo_2023) && requireNamespace("data.table", quietly = TRUE)) {

  nomes_2023 <- names(data.table::fread(arquivo_2023, nrows = 0))

  colunas_diag <- intersect(
    c(
      "sg_uf", "cd_normalizado", "no_item_monitorado",
      "no_estacao", "cod_estacao", "id_estacao", "dh_medicao"
    ),
    nomes_2023
  )

  bruto_2023_diag <- data.table::fread(
    arquivo_2023,
    select = colunas_diag,
    showProgress = TRUE
  )

  bruto_2023_diag <- as.data.frame(bruto_2023_diag)

  if ("sg_uf" %in% names(bruto_2023_diag)) {
    diag_2023_uf <- bruto_2023_diag %>%
      count(sg_uf, name = "n_registros") %>%
      arrange(desc(n_registros))

    write.csv(
      diag_2023_uf,
      file.path(pasta_saida, "diagnostico_2023_registros_por_uf.csv"),
      row.names = FALSE,
      fileEncoding = "UTF-8"
    )
  }

  if (all(c("sg_uf", "cd_normalizado") %in% names(bruto_2023_diag))) {
    diag_2023_mp10_uf <- bruto_2023_diag %>%
      filter(cd_normalizado == "MP10") %>%
      count(sg_uf, name = "n_registros_mp10") %>%
      arrange(desc(n_registros_mp10))

    diag_2023_pe_parametros <- bruto_2023_diag %>%
      filter(sg_uf == "PE") %>%
      count(cd_normalizado, name = "n_registros") %>%
      arrange(desc(n_registros))

    write.csv(
      diag_2023_mp10_uf,
      file.path(pasta_saida, "diagnostico_2023_mp10_por_uf.csv"),
      row.names = FALSE,
      fileEncoding = "UTF-8"
    )

    write.csv(
      diag_2023_pe_parametros,
      file.path(pasta_saida, "diagnostico_2023_parametros_pe.csv"),
      row.names = FALSE,
      fileEncoding = "UTF-8"
    )
  }

  if ("no_estacao" %in% names(bruto_2023_diag)) {
    padrao_suape <- "SUAPE|IPOJUCA|CUPE|IFPE|CPRH"

    diag_2023_busca_estacoes <- bruto_2023_diag %>%
      filter(grepl(padrao_suape, no_estacao, ignore.case = TRUE)) %>%
      distinct() %>%
      arrange(no_estacao)

    write.csv(
      diag_2023_busca_estacoes,
      file.path(pasta_saida, "diagnostico_2023_busca_estacoes_suape.csv"),
      row.names = FALSE,
      fileEncoding = "UTF-8"
    )
  }

  diag_2023_status$diagnostico_executado <- TRUE
  diag_2023_status$observacao <- "Diagnostico 2023 concluido com fread."

} else if (!file.exists(arquivo_2023)) {
  diag_2023_status$observacao <-
    "Arquivo bruto de 2023 nao esta disponivel na pasta local."
} else {
  diag_2023_status$observacao <-
    "Instale o pacote data.table para executar o diagnostico do arquivo bruto de 2023."
}

# ------------------------------------------------------------
# 8. SALVAR PRODUTOS DO INVENTARIO
# ------------------------------------------------------------

write.csv(
  inventario_estacao_ano,
  file.path(pasta_saida, "inventario_mp10_estacao_ano.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

write.csv(
  inventario_estacoes,
  file.path(pasta_saida, "inventario_mp10_estacoes.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

write.csv(
  metadados_2017_2022,
  file.path(pasta_saida, "metadados_mp10_2017_2022.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

write.csv(
  esquema_monitorar,
  file.path(pasta_saida, "esquema_monitorar_2023_2025.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

write.csv(
  diag_2023_status,
  file.path(pasta_saida, "diagnostico_2023_status.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# ------------------------------------------------------------
# 9. RESUMO NO CONSOLE
# ------------------------------------------------------------

cat("\n============================================================\n")
cat("ETAPA 1 - INVENTARIO DE MP10 CONCLUIDO\n")
cat("============================================================\n\n")

cat("Estacoes identificadas na base v1:\n")
print(inventario_estacoes %>% select(cod_estacao, no_estacao, anos_com_registro))

cat("\nRegistros observados por ano:\n")
print(
  dados %>%
    count(ano, name = "n_registros") %>%
    arrange(ano)
)

cat("\nAnos sem qualquer registro na base consolidada:\n")
anos_presentes <- sort(unique(dados$ano))
print(setdiff(2017:2025, anos_presentes))

cat("\nSituacao do diagnostico de 2023:\n")
print(diag_2023_status)

cat("\nArquivos gerados em: ", pasta_saida, "\n", sep = "")
cat("- inventario_mp10_estacao_ano.csv\n")
cat("- inventario_mp10_estacoes.csv\n")
cat("- metadados_mp10_2017_2022.csv\n")
cat("- esquema_monitorar_2023_2025.csv\n")
cat("- diagnostico_2023_status.csv\n")
cat("- arquivos adicionais de diagnostico de 2023, quando aplicavel\n\n")

cat("ATENCAO: cobertura_bruta_* e apenas diagnostica.\n")
cat("Os criterios formais de validade e completude serao definidos nas etapas seguintes.\n")
