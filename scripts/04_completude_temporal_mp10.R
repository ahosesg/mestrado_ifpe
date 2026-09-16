# ============================================================
# ETAPA 4 - COMPLETUDE E REPRESENTATIVIDADE TEMPORAL DO MP10
# Pernambuco, 2017-2025
# ============================================================
#
# Objetivo:
# Avaliar a completude da serie horaria analitica v2 nas escalas
# diaria, mensal, quadrimestral e anual, separando o criterio
# nacional de representatividade temporal de analises de
# sensibilidade mais restritivas.
#
# CRITERIO PRINCIPAL:
# Guia Tecnico para o Monitoramento e Avaliacao da Qualidade do Ar
# e Relatorio Anual de Acompanhamento da Qualidade do Ar 2025/MMA:
# - media diaria: pelo menos 2/3 das medias horarias validas no dia;
# - media mensal: pelo menos 2/3 das medias diarias validas no mes;
# - media anual: pelo menos 1/2 das medias diarias validas em CADA
#   quadrimestre (jan-abr, mai-ago, set-dez).
#
# Para uma serie horaria com 24 possibilidades por dia, 2/3 = 16
# horas validas. Os limiares de 18, 20 e 24 horas sao calculados
# apenas como sensibilidade, sem carater normativo neste projeto.
#
# IMPORTANTE:
# - A v2 nao e alterada.
# - Este script NAO define ainda a concentracao anual de referencia.
# - A elegibilidade final dos anos sera decidida depois da leitura
#   conjunta da completude, distribuicao temporal e sensibilidade.
# ============================================================

library(dplyr)

arquivo_v2 <- "dados_MP10_horario_v2.rds"
pasta_saida <- file.path("outputs", "04_completude")
dir.create(pasta_saida, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(arquivo_v2)) {
  stop(
    paste0(
      "Arquivo nao encontrado: ", arquivo_v2,
      ". Execute antes scripts/03b_regras_validade_base_v2.R."
    )
  )
}

# ------------------------------------------------------------
# FUNCOES AUXILIARES
# ------------------------------------------------------------

media_segura <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) == 0) return(NA_real_)
  mean(x)
}

max_run <- function(x) {
  # Maior sequencia consecutiva de TRUE.
  if (length(x) == 0 || !any(x, na.rm = TRUE)) return(0L)
  rr <- rle(replace(x, is.na(x), FALSE))
  max(rr$lengths[rr$values], na.rm = TRUE)
}

quadrimestre <- function(mes) {
  ifelse(mes <= 4, 1L, ifelse(mes <= 8, 2L, 3L))
}

# ------------------------------------------------------------
# 1. CARREGAR V2 E CHECAR ESTRUTURA
# ------------------------------------------------------------

v2 <- readRDS(arquivo_v2)

necessarias <- c(
  "ano", "cod_estacao", "no_estacao", "dh_arredondado",
  "nu_concentracao_horaria", "nu_concentracao_horaria_mediana"
)

faltantes <- setdiff(necessarias, names(v2))
if (length(faltantes) > 0) {
  stop(
    paste(
      "A v2 nao contem as colunas necessarias:",
      paste(faltantes, collapse = ", ")
    )
  )
}

v2 <- v2 %>%
  mutate(
    ano = as.integer(ano),
    cod_estacao = as.character(cod_estacao),
    no_estacao = as.character(no_estacao),
    dh_arredondado = as.POSIXct(dh_arredondado, tz = "America/Recife"),
    data = as.Date(dh_arredondado),
    mes = as.integer(format(data, "%m")),
    quadrimestre = quadrimestre(mes)
  )

# ------------------------------------------------------------
# 2. RESUMO OBSERVADO POR DIA
# ------------------------------------------------------------

# A alternativa baseada na mediana se refere somente ao tratamento
# das duplicidades divergentes. Ela usa as mesmas horas elegiveis.

diario_observado <- v2 %>%
  group_by(ano, cod_estacao, no_estacao, data) %>%
  summarise(
    n_horas_grade_observada = n(),
    n_horas_validas = sum(!is.na(nu_concentracao_horaria)),
    media_24h_principal_bruta = media_segura(nu_concentracao_horaria),
    media_24h_sens_mediana_dup_bruta = media_segura(nu_concentracao_horaria_mediana),
    .groups = "drop"
  )

# ------------------------------------------------------------
# 3. GRADE CALENDARIO COMPLETA POR ESTACAO-ANO
# ------------------------------------------------------------

estacao_ano <- v2 %>%
  distinct(ano, cod_estacao, no_estacao) %>%
  arrange(ano, cod_estacao)

lista_calendario <- lapply(seq_len(nrow(estacao_ano)), function(i) {
  a <- estacao_ano$ano[i]
  data.frame(
    ano = a,
    cod_estacao = estacao_ano$cod_estacao[i],
    no_estacao = estacao_ano$no_estacao[i],
    data = seq.Date(
      as.Date(sprintf("%04d-01-01", a)),
      as.Date(sprintf("%04d-12-31", a)),
      by = "day"
    ),
    stringsAsFactors = FALSE
  )
})

calendario <- bind_rows(lista_calendario)

completude_diaria <- calendario %>%
  left_join(
    diario_observado,
    by = c("ano", "cod_estacao", "no_estacao", "data")
  ) %>%
  mutate(
    n_horas_grade_observada = ifelse(
      is.na(n_horas_grade_observada), 0L, n_horas_grade_observada
    ),
    n_horas_validas = ifelse(is.na(n_horas_validas), 0L, n_horas_validas),
    pct_horas_validas = 100 * n_horas_validas / 24,
    dia_valido_mma_16h = n_horas_validas >= 16,
    dia_valido_sens_18h = n_horas_validas >= 18,
    dia_valido_sens_20h = n_horas_validas >= 20,
    dia_completo_24h = n_horas_validas >= 24,
    media_24h_mma = ifelse(
      dia_valido_mma_16h, media_24h_principal_bruta, NA_real_
    ),
    media_24h_mma_sens_mediana_dup = ifelse(
      dia_valido_mma_16h,
      media_24h_sens_mediana_dup_bruta,
      NA_real_
    ),
    diferenca_media_24h_dup =
      media_24h_mma - media_24h_mma_sens_mediana_dup,
    mes = as.integer(format(data, "%m")),
    quadrimestre = quadrimestre(mes)
  ) %>%
  arrange(ano, cod_estacao, data)

write.csv(
  completude_diaria,
  file.path(pasta_saida, "completude_diaria_mp10.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# ------------------------------------------------------------
# 4. COMPLETUDE MENSAL
# ------------------------------------------------------------

completude_mensal <- completude_diaria %>%
  group_by(ano, cod_estacao, no_estacao, mes) %>%
  summarise(
    n_dias_calendario = n(),
    n_dias_validos_16h = sum(dia_valido_mma_16h),
    n_dias_validos_18h = sum(dia_valido_sens_18h),
    n_dias_validos_20h = sum(dia_valido_sens_20h),
    n_dias_completos_24h = sum(dia_completo_24h),
    pct_dias_validos_16h = 100 * n_dias_validos_16h / n_dias_calendario,
    minimo_dias_mma = ceiling((2 / 3) * n_dias_calendario),
    mes_representativo_mma = n_dias_validos_16h >= minimo_dias_mma,
    media_mensal_se_representativa = ifelse(
      mes_representativo_mma,
      media_segura(media_24h_mma),
      NA_real_
    ),
    .groups = "drop"
  ) %>%
  arrange(ano, cod_estacao, mes)

write.csv(
  completude_mensal,
  file.path(pasta_saida, "completude_mensal_mp10.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# ------------------------------------------------------------
# 5. COMPLETUDE POR QUADRIMESTRE
# ------------------------------------------------------------

completude_quadrimestre <- completude_diaria %>%
  group_by(ano, cod_estacao, no_estacao, quadrimestre) %>%
  summarise(
    n_dias_calendario = n(),
    n_dias_validos_16h = sum(dia_valido_mma_16h),
    pct_dias_validos_16h = 100 * n_dias_validos_16h / n_dias_calendario,
    minimo_dias_mma = ceiling(0.5 * n_dias_calendario),
    quadrimestre_representativo_mma =
      n_dias_validos_16h >= minimo_dias_mma,
    .groups = "drop"
  ) %>%
  arrange(ano, cod_estacao, quadrimestre)

write.csv(
  completude_quadrimestre,
  file.path(pasta_saida, "completude_quadrimestre_mp10.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# ------------------------------------------------------------
# 6. RESUMO ANUAL E DISTRIBUICAO TEMPORAL
# ------------------------------------------------------------

resumo_quad <- completude_quadrimestre %>%
  group_by(ano, cod_estacao, no_estacao) %>%
  summarise(
    n_quadrimestres_representativos = sum(quadrimestre_representativo_mma),
    menor_pct_quadrimestre = min(pct_dias_validos_16h),
    ano_representativo_mma = all(quadrimestre_representativo_mma),
    .groups = "drop"
  )

resumo_mes <- completude_mensal %>%
  group_by(ano, cod_estacao, no_estacao) %>%
  summarise(
    n_meses_representativos_mma = sum(mes_representativo_mma),
    n_meses_sem_dia_valido = sum(n_dias_validos_16h == 0),
    .groups = "drop"
  )

completude_anual <- completude_diaria %>%
  group_by(ano, cod_estacao, no_estacao) %>%
  arrange(data, .by_group = TRUE) %>%
  summarise(
    n_dias_calendario = n(),
    n_dias_com_algum_valor = sum(n_horas_validas > 0),
    n_dias_validos_16h = sum(dia_valido_mma_16h),
    n_dias_validos_18h = sum(dia_valido_sens_18h),
    n_dias_validos_20h = sum(dia_valido_sens_20h),
    n_dias_completos_24h = sum(dia_completo_24h),
    pct_dias_validos_16h = 100 * n_dias_validos_16h / n_dias_calendario,
    pct_dias_validos_18h = 100 * n_dias_validos_18h / n_dias_calendario,
    pct_dias_validos_20h = 100 * n_dias_validos_20h / n_dias_calendario,
    pct_dias_completos_24h = 100 * n_dias_completos_24h / n_dias_calendario,
    maior_lacuna_dias_sem_media_valida = max_run(!dia_valido_mma_16h),
    maior_lacuna_dias_sem_dado_horario = max_run(n_horas_validas == 0),
    max_abs_diferenca_diaria_media_mediana_dup = {
      x <- abs(diferenca_media_24h_dup[is.finite(diferenca_media_24h_dup)])
      if (length(x) == 0) NA_real_ else max(x)
    },
    media_abs_diferenca_diaria_media_mediana_dup = {
      x <- abs(diferenca_media_24h_dup[is.finite(diferenca_media_24h_dup)])
      if (length(x) == 0) NA_real_ else mean(x)
    },
    .groups = "drop"
  ) %>%
  left_join(resumo_quad, by = c("ano", "cod_estacao", "no_estacao")) %>%
  left_join(resumo_mes, by = c("ano", "cod_estacao", "no_estacao")) %>%
  arrange(ano, cod_estacao)

write.csv(
  completude_anual,
  file.path(pasta_saida, "completude_anual_mp10.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# ------------------------------------------------------------
# 7. CRITERIOS DOCUMENTADOS
# ------------------------------------------------------------

criterios <- data.frame(
  nivel = c(
    "diario_principal",
    "mensal_principal",
    "anual_principal",
    "diario_sensibilidade_18h",
    "diario_sensibilidade_20h",
    "diario_sensibilidade_24h"
  ),
  criterio = c(
    ">= 16 medias horarias validas em 24 h (2/3)",
    ">= 2/3 das medias diarias validas no mes",
    ">= 1/2 das medias diarias validas em cada quadrimestre",
    ">= 18 horas validas; analise de sensibilidade",
    ">= 20 horas validas; analise de sensibilidade",
    "24 horas validas; analise de sensibilidade"
  ),
  uso = c(
    "criterio nacional de representatividade temporal",
    "criterio nacional de representatividade temporal",
    "criterio nacional de representatividade temporal",
    "sensibilidade; nao normativo neste projeto",
    "sensibilidade; nao normativo neste projeto",
    "sensibilidade; nao normativo neste projeto"
  ),
  stringsAsFactors = FALSE
)

write.csv(
  criterios,
  file.path(pasta_saida, "criterios_completude_documentados.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# ------------------------------------------------------------
# 8. RESUMO FINAL
# ------------------------------------------------------------

resumo_execucao <- data.frame(
  indicador = c(
    "n_estacao_ano_avaliados",
    "n_estacao_ano_representativos_mma",
    "n_estacao_ano_nao_representativos_mma",
    "n_dias_validos_16h_total",
    "n_dias_validos_18h_total",
    "n_dias_validos_20h_total"
  ),
  valor = c(
    nrow(completude_anual),
    sum(completude_anual$ano_representativo_mma),
    sum(!completude_anual$ano_representativo_mma),
    sum(completude_anual$n_dias_validos_16h),
    sum(completude_anual$n_dias_validos_18h),
    sum(completude_anual$n_dias_validos_20h)
  )
)

write.csv(
  resumo_execucao,
  file.path(pasta_saida, "resumo_execucao_completude.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

cat("\n============================================================\n")
cat("ETAPA 4 - COMPLETUDE TEMPORAL CONCLUIDA\n")
cat("============================================================\n\n")
print(resumo_execucao)
cat("\nResumo anual:\n")
print(completude_anual)
cat("\nSaidas em:", pasta_saida, "\n")
cat("\nPROXIMO PASSO: selecionar estacao-anos elegiveis e construir\n")
cat("as concentracoes anuais de referencia com analise de sensibilidade.\n")