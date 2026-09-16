# ============================================================
# ETAPA 5 - CONCENTRACOES ANUAIS DE REFERENCIA DE MP10
# Pernambuco, 2017-2025
# ============================================================
#
# Objetivo:
# Construir concentracoes anuais de referencia por estacao-ano a
# partir das medias diarias validas, usando como criterio principal
# de elegibilidade anual a representatividade temporal definida na
# Etapa 4. Tambem calcula sensibilidades com limiares diarios de
# 18 h, 20 h e 24 h, sem substituir o criterio principal de 16 h.
#
# PRINCIPIOS:
# - A referencia principal e calculada por ESTACAO-ANO.
# - Cada dia valido recebe peso igual na media anual.
# - O criterio principal de dia valido e >=16 horas validas.
# - O ano principal so recebe referencia se TODOS os quadrimestres
#   tiverem pelo menos 50% dos dias validos sob o criterio de 16 h.
# - As referencias de 18 h, 20 h e 24 h sao sensibilidades e tem
#   sua propria verificacao quadrimestral neste script.
# - Nao e calculada media regional combinando estacoes.
# ============================================================

library(dplyr)

arquivo_diario <- file.path("outputs", "04_completude", "completude_diaria_mp10.csv")
arquivo_anual <- file.path("outputs", "04_completude", "completude_anual_mp10.csv")
pasta_saida <- file.path("outputs", "05_referencias_anuais")
dir.create(pasta_saida, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(arquivo_diario)) stop("Arquivo diario da Etapa 4 nao encontrado.")
if (!file.exists(arquivo_anual)) stop("Arquivo anual da Etapa 4 nao encontrado.")

diario <- read.csv(arquivo_diario, stringsAsFactors = FALSE, check.names = FALSE)
anual <- read.csv(arquivo_anual, stringsAsFactors = FALSE, check.names = FALSE)

diario$data <- as.Date(diario$data)
diario$ano <- as.integer(diario$ano)
diario$cod_estacao <- as.character(diario$cod_estacao)
diario$no_estacao <- as.character(diario$no_estacao)
diario$quadrimestre <- as.integer(diario$quadrimestre)

# Checagem minima de estrutura.
necessarias <- c(
  "ano", "cod_estacao", "no_estacao", "data", "quadrimestre",
  "n_horas_validas", "dia_valido_mma_16h", "dia_valido_sens_18h",
  "dia_valido_sens_20h", "dia_completo_24h",
  "media_24h_principal_bruta", "media_24h_mma",
  "media_24h_mma_sens_mediana_dup"
)
faltantes <- setdiff(necessarias, names(diario))
if (length(faltantes) > 0) {
  stop(paste("Colunas ausentes em completude_diaria_mp10.csv:", paste(faltantes, collapse = ", ")))
}

media_segura <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) == 0) return(NA_real_)
  mean(x)
}

# ------------------------------------------------------------
# 1. REPRESENTATIVIDADE QUADRIMESTRAL POR LIMIAR DIARIO
# ------------------------------------------------------------

quad_sens <- diario %>%
  group_by(ano, cod_estacao, no_estacao, quadrimestre) %>%
  summarise(
    n_dias_calendario = n(),
    minimo_50pct = ceiling(0.5 * n_dias_calendario),
    n_dias_16h = sum(dia_valido_mma_16h, na.rm = TRUE),
    n_dias_18h = sum(dia_valido_sens_18h, na.rm = TRUE),
    n_dias_20h = sum(dia_valido_sens_20h, na.rm = TRUE),
    n_dias_24h = sum(dia_completo_24h, na.rm = TRUE),
    rep_quad_16h = n_dias_16h >= minimo_50pct,
    rep_quad_18h = n_dias_18h >= minimo_50pct,
    rep_quad_20h = n_dias_20h >= minimo_50pct,
    rep_quad_24h = n_dias_24h >= minimo_50pct,
    .groups = "drop"
  )

write.csv(
  quad_sens,
  file.path(pasta_saida, "referencia_representatividade_quadrimestral_sensibilidade.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

rep_anual_sens <- quad_sens %>%
  group_by(ano, cod_estacao, no_estacao) %>%
  summarise(
    ano_rep_16h = all(rep_quad_16h),
    ano_rep_18h = all(rep_quad_18h),
    ano_rep_20h = all(rep_quad_20h),
    ano_rep_24h = all(rep_quad_24h),
    menor_pct_quad_16h = min(100 * n_dias_16h / n_dias_calendario),
    menor_pct_quad_18h = min(100 * n_dias_18h / n_dias_calendario),
    menor_pct_quad_20h = min(100 * n_dias_20h / n_dias_calendario),
    menor_pct_quad_24h = min(100 * n_dias_24h / n_dias_calendario),
    .groups = "drop"
  )

# ------------------------------------------------------------
# 2. REFERENCIAS ANUAIS POR LIMIAR
# ------------------------------------------------------------

referencias <- diario %>%
  group_by(ano, cod_estacao, no_estacao) %>%
  summarise(
    n_dias_ref_16h = sum(dia_valido_mma_16h, na.rm = TRUE),
    n_dias_ref_18h = sum(dia_valido_sens_18h, na.rm = TRUE),
    n_dias_ref_20h = sum(dia_valido_sens_20h, na.rm = TRUE),
    n_dias_ref_24h = sum(dia_completo_24h, na.rm = TRUE),
    ref_16h_bruta = media_segura(media_24h_principal_bruta[dia_valido_mma_16h]),
    ref_18h_bruta = media_segura(media_24h_principal_bruta[dia_valido_sens_18h]),
    ref_20h_bruta = media_segura(media_24h_principal_bruta[dia_valido_sens_20h]),
    ref_24h_bruta = media_segura(media_24h_principal_bruta[dia_completo_24h]),
    ref_16h_mediana_dup_bruta = media_segura(media_24h_mma_sens_mediana_dup[dia_valido_mma_16h]),
    .groups = "drop"
  ) %>%
  left_join(rep_anual_sens, by = c("ano", "cod_estacao", "no_estacao")) %>%
  mutate(
    referencia_anual_principal = ifelse(ano_rep_16h, ref_16h_bruta, NA_real_),
    referencia_anual_sens_18h = ifelse(ano_rep_18h, ref_18h_bruta, NA_real_),
    referencia_anual_sens_20h = ifelse(ano_rep_20h, ref_20h_bruta, NA_real_),
    referencia_anual_sens_24h = ifelse(ano_rep_24h, ref_24h_bruta, NA_real_),
    referencia_16h_sens_mediana_dup = ifelse(ano_rep_16h, ref_16h_mediana_dup_bruta, NA_real_),
    dif_abs_ref_media_vs_mediana_dup = abs(referencia_anual_principal - referencia_16h_sens_mediana_dup),
    dif_pct_ref_media_vs_mediana_dup = 100 * dif_abs_ref_media_vs_mediana_dup / referencia_anual_principal,
    dif_pct_ref_18h_vs_16h = 100 * (referencia_anual_sens_18h - referencia_anual_principal) / referencia_anual_principal,
    dif_pct_ref_20h_vs_16h = 100 * (referencia_anual_sens_20h - referencia_anual_principal) / referencia_anual_principal,
    dif_pct_ref_24h_vs_16h = 100 * (referencia_anual_sens_24h - referencia_anual_principal) / referencia_anual_principal
  ) %>%
  arrange(ano, cod_estacao)

write.csv(
  referencias,
  file.path(pasta_saida, "referencias_anuais_estacao_ano.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

referencias_principais <- referencias %>%
  filter(ano_rep_16h) %>%
  select(
    ano, cod_estacao, no_estacao,
    n_dias_ref_16h, menor_pct_quad_16h,
    referencia_anual_principal,
    referencia_16h_sens_mediana_dup,
    dif_abs_ref_media_vs_mediana_dup,
    dif_pct_ref_media_vs_mediana_dup,
    ano_rep_18h, ano_rep_20h, ano_rep_24h,
    referencia_anual_sens_18h,
    referencia_anual_sens_20h,
    referencia_anual_sens_24h,
    dif_pct_ref_18h_vs_16h,
    dif_pct_ref_20h_vs_16h,
    dif_pct_ref_24h_vs_16h
  )

write.csv(
  referencias_principais,
  file.path(pasta_saida, "referencias_anuais_elegiveis_principal.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# ------------------------------------------------------------
# 3. CANDIDATOS AO PRE-TESTE
# ------------------------------------------------------------

candidatos_preteste <- anual %>%
  filter(ano_representativo_mma) %>%
  select(
    ano, cod_estacao, no_estacao,
    n_dias_validos_16h, pct_dias_validos_16h,
    maior_lacuna_dias_sem_media_valida,
    menor_pct_quadrimestre,
    n_meses_representativos_mma,
    n_meses_sem_dia_valido
  ) %>%
  left_join(
    referencias_principais %>%
      select(ano, cod_estacao, no_estacao, referencia_anual_principal),
    by = c("ano", "cod_estacao", "no_estacao")
  ) %>%
  arrange(
    n_meses_sem_dia_valido,
    desc(n_meses_representativos_mma),
    desc(menor_pct_quadrimestre),
    maior_lacuna_dias_sem_media_valida,
    desc(pct_dias_validos_16h)
  )

write.csv(
  candidatos_preteste,
  file.path(pasta_saida, "candidatos_preteste_mp10.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# ------------------------------------------------------------
# 4. DOCUMENTACAO DA DEFINICAO OPERACIONAL
# ------------------------------------------------------------

definicao <- data.frame(
  elemento = c(
    "unidade_analitica",
    "media_diaria_principal",
    "elegibilidade_anual_principal",
    "referencia_anual_principal",
    "ponderacao",
    "sensibilidade_duplicidades",
    "sensibilidade_completude",
    "media_regional"
  ),
  definicao = c(
    "estacao-ano",
    "media das concentracoes horarias elegiveis quando houver >=16 horas validas no dia",
    "pelo menos 50% das medias diarias validas em cada um dos tres quadrimestres",
    "media aritmetica das medias diarias validas do estacao-ano elegivel",
    "peso igual para cada dia valido; dias nao sao ponderados pelo numero de horas acima do minimo",
    "recalculo usando mediana no tratamento das duplicidades divergentes",
    "recalculo com criterios diarios de 18 h, 20 h e 24 h e verificacao quadrimestral correspondente",
    "nao calculada nesta etapa; cada estacao permanece independente"
  ),
  stringsAsFactors = FALSE
)

write.csv(
  definicao,
  file.path(pasta_saida, "definicao_operacional_referencia_anual.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# ------------------------------------------------------------
# 5. RESUMO FINAL
# ------------------------------------------------------------

resumo <- data.frame(
  indicador = c(
    "n_estacao_ano_total",
    "n_referencias_principais",
    "n_referencias_18h",
    "n_referencias_20h",
    "n_referencias_24h"
  ),
  valor = c(
    nrow(referencias),
    sum(referencias$ano_rep_16h),
    sum(referencias$ano_rep_18h),
    sum(referencias$ano_rep_20h),
    sum(referencias$ano_rep_24h)
  )
)

write.csv(
  resumo,
  file.path(pasta_saida, "resumo_execucao_referencias.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

cat("\n============================================================\n")
cat("ETAPA 5 - REFERENCIAS ANUAIS DE MP10 CONCLUIDA\n")
cat("============================================================\n\n")
print(resumo)
cat("\nReferencias principais elegiveis:\n")
print(referencias_principais %>% select(ano, cod_estacao, no_estacao, referencia_anual_principal))
cat("\nSaidas em:", pasta_saida, "\n")
