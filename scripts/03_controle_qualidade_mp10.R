# ============================================================
# ETAPA 3 - DIAGNOSTICO DE CONTROLE DE QUALIDADE DO MP10
# Pernambuco, 2017-2025
# ============================================================
#
# Objetivo:
# Auditar a base consolidada v1 antes de definir regras formais
# de validade. O script identifica valores ausentes, negativos e
# nulos, duplicidades temporais, colisoes provocadas pelo
# arredondamento horario, registros provenientes de multiplas
# fontes, combinacoes de flags e inconsistencias temporais.
#
# IMPORTANTE:
# - Este script NAO exclui registros.
# - Valores altos NAO sao classificados automaticamente como
#   invalidos.
# - Valores negativos e iguais a zero sao apenas identificados
#   para investigacao nesta etapa.
# - As flags do MonitorAr sao descritas como encontradas na base;
#   sua interpretacao como valida/invalida sera congelada somente
#   depois da auditoria dos resultados e da documentacao da fonte.
# - A base v1 permanece inalterada.
# ============================================================

library(dplyr)

# ------------------------------------------------------------
# 0. CONFIGURACAO
# ------------------------------------------------------------

arquivo_base <- "dados_MP10_2017_2025_PE_v1.rds"
pasta_saida <- file.path("outputs", "03_controle_qualidade")
dir.create(pasta_saida, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(arquivo_base)) {
  stop(
    paste0(
      "Arquivo nao encontrado: ", arquivo_base,
      ". Execute o script a partir da raiz do projeto Mestrado.Rproj."
    )
  )
}

# ------------------------------------------------------------
# FUNCOES AUXILIARES
# ------------------------------------------------------------

colapsar_unicos <- function(x) {
  x <- unique(as.character(x[!is.na(x) & trimws(as.character(x)) != ""]))
  if (length(x) == 0) return(NA_character_)
  paste(sort(x), collapse = " | ")
}

texto_na <- function(x) {
  x <- as.character(x)
  x[is.na(x) | trimws(x) == ""] <- "<NA>"
  x
}

quantil_seguro <- function(x, p) {
  x <- x[is.finite(x)]
  if (length(x) == 0) return(NA_real_)
  as.numeric(quantile(x, probs = p, na.rm = TRUE, names = FALSE, type = 7))
}

min_seguro <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) == 0) return(NA_real_)
  min(x)
}

max_seguro <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) == 0) return(NA_real_)
  max(x)
}

media_segura <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) == 0) return(NA_real_)
  mean(x)
}

dp_seguro <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) < 2) return(NA_real_)
  sd(x)
}

# ------------------------------------------------------------
# 1. CARREGAMENTO E CHECAGEM DA BASE V1
# ------------------------------------------------------------

dados <- readRDS(arquivo_base)

colunas_obrigatorias <- c(
  "ano", "cod_estacao", "no_estacao", "nu_concentracao",
  "dh_medicao", "data_fechada", "hora_fechada"
)

faltantes <- setdiff(colunas_obrigatorias, names(dados))
if (length(faltantes) > 0) {
  stop(
    paste(
      "A base v1 nao contem as colunas obrigatorias:",
      paste(faltantes, collapse = ", ")
    )
  )
}

# Colunas opcionais esperadas nas bases MonitorAr.
for (nm in c(
  "dh_arredondado", "no_fonte_dados", "st_situacao",
  "cd_flag", "ds_flag", "st_situacao_iqar", "st_estacao"
)) {
  if (!nm %in% names(dados)) dados[[nm]] <- NA
}

# Padronizacao apenas em memoria, sem alterar o arquivo v1.
dados <- dados %>%
  mutate(
    id_linha_v1 = row_number(),
    ano = as.integer(ano),
    cod_estacao = as.character(cod_estacao),
    no_estacao = as.character(no_estacao),
    nu_concentracao = suppressWarnings(as.numeric(nu_concentracao)),
    dh_medicao = as.character(dh_medicao),
    data_fechada = as.Date(data_fechada),
    hora_fechada = as.character(hora_fechada),
    no_fonte_dados = as.character(no_fonte_dados),
    st_situacao = as.character(st_situacao),
    cd_flag = as.character(cd_flag),
    ds_flag = as.character(ds_flag),
    st_situacao_iqar = as.character(st_situacao_iqar),
    st_estacao = as.character(st_estacao)
  )

# Se dh_arredondado nao estiver utilizavel, reconstruir apenas
# para fins diagnosticos a partir de data_fechada + hora_fechada.
if (all(is.na(dados$dh_arredondado))) {
  dados$dh_arredondado <- as.POSIXct(
    paste(dados$data_fechada, dados$hora_fechada),
    format = "%Y-%m-%d %H:%M:%S",
    tz = "America/Recife"
  )
} else {
  dados$dh_arredondado <- as.POSIXct(
    dados$dh_arredondado,
    tz = "America/Recife"
  )
}

# Interpretacao do horario original somente para auditoria da
# transformacao temporal. O campo original e preservado.
dados$dh_original_posix <- as.POSIXct(
  dados$dh_medicao,
  format = "%Y-%m-%d %H:%M:%OS",
  tz = "America/Recife"
)

# ------------------------------------------------------------
# 2. CLASSIFICACAO DIAGNOSTICA DAS CONCENTRACOES
# ------------------------------------------------------------

dados <- dados %>%
  mutate(
    classe_concentracao = case_when(
      is.na(nu_concentracao) ~ "ausente",
      nu_concentracao < 0 ~ "negativa",
      nu_concentracao == 0 ~ "zero",
      nu_concentracao > 0 ~ "positiva",
      TRUE ~ "nao_classificada"
    )
  )

resumo_estacao_ano <- dados %>%
  group_by(ano, cod_estacao, no_estacao) %>%
  summarise(
    n_total = n(),
    n_ausente = sum(is.na(nu_concentracao)),
    n_negativa = sum(nu_concentracao < 0, na.rm = TRUE),
    n_zero = sum(nu_concentracao == 0, na.rm = TRUE),
    n_positiva = sum(nu_concentracao > 0, na.rm = TRUE),
    pct_ausente = 100 * n_ausente / n_total,
    pct_negativa = 100 * n_negativa / n_total,
    pct_zero = 100 * n_zero / n_total,
    pct_positiva = 100 * n_positiva / n_total,
    n_fontes = n_distinct(no_fonte_dados[!is.na(no_fonte_dados)]),
    fontes = colapsar_unicos(no_fonte_dados),
    .groups = "drop"
  ) %>%
  arrange(ano, cod_estacao)

write.csv(
  resumo_estacao_ano,
  file.path(pasta_saida, "qa_resumo_estacao_ano.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# Exporta somente os registros nao positivos/ausentes para
# inspecao posterior. Nenhum deles e excluido nesta etapa.
registros_nao_positivos <- dados %>%
  filter(is.na(nu_concentracao) | nu_concentracao <= 0) %>%
  select(
    id_linha_v1, ano, cod_estacao, no_estacao,
    dh_medicao, dh_arredondado, nu_concentracao,
    classe_concentracao, no_fonte_dados,
    st_situacao, cd_flag, ds_flag, st_situacao_iqar
  ) %>%
  arrange(ano, cod_estacao, dh_medicao)

write.csv(
  registros_nao_positivos,
  file.path(pasta_saida, "qa_registros_nao_positivos.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# ------------------------------------------------------------
# 3. DISTRIBUICAO DAS CONCENTRACOES POR ESTACAO E ANO
# ------------------------------------------------------------

# Os percentis sao descritivos. Nenhum percentil ou limite e
# utilizado como criterio automatico de exclusao.
distribuicao <- dados %>%
  group_by(ano, cod_estacao, no_estacao) %>%
  summarise(
    n_numerico = sum(!is.na(nu_concentracao)),
    minimo = min_seguro(nu_concentracao),
    p001 = quantil_seguro(nu_concentracao, 0.001),
    p01 = quantil_seguro(nu_concentracao, 0.01),
    p05 = quantil_seguro(nu_concentracao, 0.05),
    p25 = quantil_seguro(nu_concentracao, 0.25),
    mediana = quantil_seguro(nu_concentracao, 0.50),
    media = media_segura(nu_concentracao),
    dp = dp_seguro(nu_concentracao),
    p75 = quantil_seguro(nu_concentracao, 0.75),
    p95 = quantil_seguro(nu_concentracao, 0.95),
    p99 = quantil_seguro(nu_concentracao, 0.99),
    p999 = quantil_seguro(nu_concentracao, 0.999),
    maximo = max_seguro(nu_concentracao),
    .groups = "drop"
  ) %>%
  arrange(ano, cod_estacao)

write.csv(
  distribuicao,
  file.path(pasta_saida, "qa_distribuicao_concentracoes.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# ------------------------------------------------------------
# 4. DUPLICIDADES NO TIMESTAMP ORIGINAL
# ------------------------------------------------------------

# Uma mesma estacao deveria, em principio, possuir uma observacao
# por instante de medicao. Grupos com mais de uma linha sao
# descritos sem eliminacao automatica.
duplicidades_original <- dados %>%
  filter(!is.na(dh_medicao), trimws(dh_medicao) != "") %>%
  group_by(ano, cod_estacao, no_estacao, dh_medicao) %>%
  summarise(
    n_linhas = n(),
    n_valores_distintos = n_distinct(nu_concentracao, na.rm = FALSE),
    valores = colapsar_unicos(nu_concentracao),
    n_fontes_distintas = n_distinct(no_fonte_dados[!is.na(no_fonte_dados)]),
    fontes = colapsar_unicos(no_fonte_dados),
    flags = colapsar_unicos(cd_flag),
    situacoes = colapsar_unicos(st_situacao),
    .groups = "drop"
  ) %>%
  filter(n_linhas > 1) %>%
  mutate(
    valores_concordantes = n_valores_distintos <= 1
  ) %>%
  arrange(ano, cod_estacao, dh_medicao)

write.csv(
  duplicidades_original,
  file.path(pasta_saida, "qa_duplicidades_timestamp_original.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# ------------------------------------------------------------
# 5. DUPLICIDADES/COLISOES APOS ARREDONDAMENTO PARA HORA CHEIA
# ------------------------------------------------------------

duplicidades_arredondadas <- dados %>%
  filter(!is.na(dh_arredondado)) %>%
  group_by(ano, cod_estacao, no_estacao, dh_arredondado) %>%
  summarise(
    n_linhas = n(),
    n_timestamps_originais = n_distinct(dh_medicao),
    timestamps_originais = colapsar_unicos(dh_medicao),
    n_valores_distintos = n_distinct(nu_concentracao, na.rm = FALSE),
    valores = colapsar_unicos(nu_concentracao),
    n_fontes_distintas = n_distinct(no_fonte_dados[!is.na(no_fonte_dados)]),
    fontes = colapsar_unicos(no_fonte_dados),
    .groups = "drop"
  ) %>%
  filter(n_linhas > 1) %>%
  mutate(
    colisao_por_arredondamento = n_timestamps_originais > 1,
    valores_concordantes = n_valores_distintos <= 1
  ) %>%
  arrange(ano, cod_estacao, dh_arredondado)

write.csv(
  duplicidades_arredondadas,
  file.path(pasta_saida, "qa_duplicidades_hora_arredondada.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

colisoes_arredondamento <- duplicidades_arredondadas %>%
  filter(colisao_por_arredondamento)

write.csv(
  colisoes_arredondamento,
  file.path(pasta_saida, "qa_colisoes_arredondamento.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# ------------------------------------------------------------
# 6. MULTIPLAS FONTES PARA O MESMO HORARIO
# ------------------------------------------------------------

# Particularmente relevante para 2024, quando o inventario
# anterior mostrou registros com no_fonte_dados CPRH e N/A.
multiplas_fontes <- dados %>%
  filter(!is.na(dh_arredondado), !is.na(no_fonte_dados)) %>%
  group_by(ano, cod_estacao, no_estacao, dh_arredondado) %>%
  summarise(
    n_linhas = n(),
    n_fontes = n_distinct(no_fonte_dados),
    fontes = colapsar_unicos(no_fonte_dados),
    n_valores_distintos = n_distinct(nu_concentracao, na.rm = FALSE),
    valores = colapsar_unicos(nu_concentracao),
    .groups = "drop"
  ) %>%
  filter(n_fontes > 1) %>%
  mutate(
    valores_concordantes = n_valores_distintos <= 1
  ) %>%
  arrange(ano, cod_estacao, dh_arredondado)

write.csv(
  multiplas_fontes,
  file.path(pasta_saida, "qa_multiplas_fontes_mesmo_horario.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

fontes_por_ano <- dados %>%
  mutate(fonte_exibicao = texto_na(no_fonte_dados)) %>%
  count(ano, cod_estacao, no_estacao, fonte_exibicao, name = "n_registros") %>%
  arrange(ano, cod_estacao, desc(n_registros))

write.csv(
  fontes_por_ano,
  file.path(pasta_saida, "qa_fontes_por_estacao_ano.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# ------------------------------------------------------------
# 7. INVENTARIO DAS FLAGS E SITUACOES DISPONIVEIS
# ------------------------------------------------------------

flags <- dados %>%
  mutate(
    st_situacao = texto_na(st_situacao),
    cd_flag = texto_na(cd_flag),
    ds_flag = texto_na(ds_flag),
    st_situacao_iqar = texto_na(st_situacao_iqar),
    st_estacao = texto_na(st_estacao)
  ) %>%
  group_by(
    ano, cod_estacao, no_estacao,
    st_situacao, cd_flag, ds_flag, st_situacao_iqar, st_estacao
  ) %>%
  summarise(
    n_registros = n(),
    n_ausente = sum(is.na(nu_concentracao)),
    n_negativa = sum(nu_concentracao < 0, na.rm = TRUE),
    n_zero = sum(nu_concentracao == 0, na.rm = TRUE),
    n_positiva = sum(nu_concentracao > 0, na.rm = TRUE),
    minimo = min_seguro(nu_concentracao),
    maximo = max_seguro(nu_concentracao),
    .groups = "drop"
  ) %>%
  arrange(ano, cod_estacao, desc(n_registros))

write.csv(
  flags,
  file.path(pasta_saida, "qa_flags_e_situacoes.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# ------------------------------------------------------------
# 8. INTEGRIDADE TEMPORAL E EFEITO DO ARREDONDAMENTO
# ------------------------------------------------------------

dados <- dados %>%
  mutate(
    ano_data = suppressWarnings(as.integer(format(data_fechada, "%Y"))),
    diferenca_arredondamento_seg = as.numeric(
      difftime(dh_arredondado, dh_original_posix, units = "secs")
    ),
    ano_incompativel = !is.na(ano_data) & ano_data != ano
  )

integridade_temporal <- dados %>%
  group_by(ano, cod_estacao, no_estacao) %>%
  summarise(
    n_total = n(),
    n_dh_original_na = sum(is.na(dh_original_posix)),
    n_dh_arredondado_na = sum(is.na(dh_arredondado)),
    n_data_fechada_na = sum(is.na(data_fechada)),
    n_ano_incompativel = sum(ano_incompativel, na.rm = TRUE),
    n_arredondamento_0s = sum(diferenca_arredondamento_seg == 0, na.rm = TRUE),
    n_arredondamento_1s = sum(abs(diferenca_arredondamento_seg) == 1, na.rm = TRUE),
    n_arredondamento_maior_1s = sum(abs(diferenca_arredondamento_seg) > 1, na.rm = TRUE),
    max_abs_arredondamento_seg = max_seguro(abs(diferenca_arredondamento_seg)),
    .groups = "drop"
  ) %>%
  arrange(ano, cod_estacao)

write.csv(
  integridade_temporal,
  file.path(pasta_saida, "qa_integridade_temporal_estacao_ano.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# ------------------------------------------------------------
# 9. RESUMO DOS EVENTOS DE QA/QC POR ESTACAO E ANO
# ------------------------------------------------------------

resumo_dup_original <- duplicidades_original %>%
  group_by(ano, cod_estacao, no_estacao) %>%
  summarise(
    n_horarios_duplicados_original = n(),
    n_horarios_duplicados_valor_divergente = sum(!valores_concordantes),
    .groups = "drop"
  )

resumo_dup_arred <- duplicidades_arredondadas %>%
  group_by(ano, cod_estacao, no_estacao) %>%
  summarise(
    n_horas_duplicadas_arredondadas = n(),
    n_colisoes_arredondamento = sum(colisao_por_arredondamento),
    n_horas_arredondadas_valor_divergente = sum(!valores_concordantes),
    .groups = "drop"
  )

resumo_multifonte <- multiplas_fontes %>%
  group_by(ano, cod_estacao, no_estacao) %>%
  summarise(
    n_horas_multiplas_fontes = n(),
    n_multifonte_valor_divergente = sum(!valores_concordantes),
    .groups = "drop"
  )

resumo_qaqc <- resumo_estacao_ano %>%
  left_join(
    resumo_dup_original,
    by = c("ano", "cod_estacao", "no_estacao")
  ) %>%
  left_join(
    resumo_dup_arred,
    by = c("ano", "cod_estacao", "no_estacao")
  ) %>%
  left_join(
    resumo_multifonte,
    by = c("ano", "cod_estacao", "no_estacao")
  ) %>%
  mutate(
    across(
      starts_with("n_horarios_duplicados") |
        starts_with("n_horas_duplicadas") |
        starts_with("n_colisoes") |
        starts_with("n_horas_arredondadas") |
        starts_with("n_horas_multiplas") |
        starts_with("n_multifonte"),
      ~ ifelse(is.na(.x), 0L, .x)
    )
  ) %>%
  arrange(ano, cod_estacao)

write.csv(
  resumo_qaqc,
  file.path(pasta_saida, "qa_resumo_qaqc_estacao_ano.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# ------------------------------------------------------------
# 10. RESUMO GERAL DA EXECUCAO
# ------------------------------------------------------------

resumo_geral <- data.frame(
  indicador = c(
    "n_registros_v1",
    "n_concentracoes_ausentes",
    "n_concentracoes_negativas",
    "n_concentracoes_zero",
    "n_grupos_duplicados_timestamp_original",
    "n_grupos_duplicados_hora_arredondada",
    "n_colisoes_por_arredondamento",
    "n_grupos_multiplas_fontes_mesmo_horario",
    "n_combinacoes_flags_situacoes",
    "n_registros_ano_incompativel",
    "n_registros_arredondamento_maior_1s"
  ),
  valor = c(
    nrow(dados),
    sum(is.na(dados$nu_concentracao)),
    sum(dados$nu_concentracao < 0, na.rm = TRUE),
    sum(dados$nu_concentracao == 0, na.rm = TRUE),
    nrow(duplicidades_original),
    nrow(duplicidades_arredondadas),
    nrow(colisoes_arredondamento),
    nrow(multiplas_fontes),
    nrow(flags),
    sum(dados$ano_incompativel, na.rm = TRUE),
    sum(abs(dados$diferenca_arredondamento_seg) > 1, na.rm = TRUE)
  )
)

write.csv(
  resumo_geral,
  file.path(pasta_saida, "qa_resumo_geral.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# ------------------------------------------------------------
# 11. SAIDA NO CONSOLE
# ------------------------------------------------------------

cat("\n============================================================\n")
cat("ETAPA 3 - DIAGNOSTICO DE CONTROLE DE QUALIDADE CONCLUIDO\n")
cat("============================================================\n\n")
cat("A base v1 NAO foi alterada.\n")
cat("Nenhum registro foi excluido nesta etapa.\n\n")
print(resumo_geral)
cat("\nArquivos produzidos em:", pasta_saida, "\n")
print(list.files(pasta_saida))
cat("\nPROXIMO PASSO: interpretar estes diagnosticos e somente entao\n")
cat("congelar as regras de validade que serao usadas para gerar a v2.\n")
