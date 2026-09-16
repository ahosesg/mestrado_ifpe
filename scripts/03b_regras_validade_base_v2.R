# ============================================================
# ETAPA 3B - REGRAS DE VALIDADE E CONSTRUCAO DA BASE ANALITICA V2
# Pernambuco, 2017-2025
# ============================================================
#
# Objetivo:
# Aplicar regras explicitas e rastreaveis de elegibilidade aos
# registros da base v1, resolver duplicidades horarias sem dar
# peso adicional a repeticoes identicas e gerar uma serie horaria
# analitica (v2) para a etapa seguinte de completude.
#
# PRINCIPIOS:
# 1. A base v1 permanece inalterada.
# 2. Ausencias nao entram no valor horario analitico.
# 3. Nos dados MonitorAr, registros com st_situacao == "IN" nao
#    entram no valor horario analitico.
# 4. Valores iguais a zero nao sao excluidos automaticamente.
# 5. Valores altos nao sao excluidos automaticamente.
# 6. Para series historicas sem flag de validacao, valores numericos
#    nao ausentes permanecem elegiveis provisoriamente.
# 7. Duplicidades concordantes colapsam para um unico valor horario.
# 8. Duplicidades divergentes sao agregadas pela media aritmetica
#    dos VALORES DISTINTOS elegiveis, evitando sobrepeso de copias
#    repetidas. A mediana dos valores distintos e preservada como
#    analise de sensibilidade.
#
# IMPORTANTE:
# A classificacao "elegivel" neste script e uma regra analitica
# para o projeto, nao uma declaracao de validacao regulatoria do
# dado original.
# ============================================================

library(dplyr)

arquivo_base <- "dados_MP10_2017_2025_PE_v1.rds"
pasta_saida <- file.path("outputs", "03b_regras_validade")
dir.create(pasta_saida, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(arquivo_base)) {
  stop(paste0("Arquivo nao encontrado: ", arquivo_base))
}

# ------------------------------------------------------------
# 1. CARREGAR E PREPARAR A V1
# ------------------------------------------------------------

dados <- readRDS(arquivo_base)

for (nm in c("st_situacao", "cd_flag", "ds_flag", "no_fonte_dados")) {
  if (!nm %in% names(dados)) dados[[nm]] <- NA_character_
}

if (!"dh_arredondado" %in% names(dados)) {
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

dados <- dados %>%
  mutate(
    id_linha_v1 = row_number(),
    ano = as.integer(ano),
    cod_estacao = as.character(cod_estacao),
    no_estacao = as.character(no_estacao),
    nu_concentracao = suppressWarnings(as.numeric(nu_concentracao)),
    st_situacao = as.character(st_situacao),
    cd_flag = as.character(cd_flag),
    ds_flag = as.character(ds_flag),
    no_fonte_dados = as.character(no_fonte_dados)
  )

# ------------------------------------------------------------
# 2. REGRAS DE ELEGIBILIDADE POR REGISTRO
# ------------------------------------------------------------

dados_classificados <- dados %>%
  mutate(
    regra_validade = case_when(
      is.na(nu_concentracao) ~ "INVALIDO_AUSENTE",
      !is.na(st_situacao) & st_situacao == "IN" ~ "INVALIDO_STATUS_FONTE",
      !is.na(st_situacao) & st_situacao == "VA" ~ "ELEGIVEL_STATUS_VA",
      is.na(st_situacao) & ano <= 2022 ~ "ELEGIVEL_SEM_FLAG_HISTORICA",
      TRUE ~ "REVISAR_STATUS"
    ),
    elegivel_analise = regra_validade %in% c(
      "ELEGIVEL_STATUS_VA",
      "ELEGIVEL_SEM_FLAG_HISTORICA"
    ),
    flag_zero = !is.na(nu_concentracao) & nu_concentracao == 0,
    flag_negativo = !is.na(nu_concentracao) & nu_concentracao < 0
  )

# ------------------------------------------------------------
# 3. RESUMO DAS REGRAS APLICADAS
# ------------------------------------------------------------

resumo_regras <- dados_classificados %>%
  count(
    ano, cod_estacao, no_estacao, regra_validade,
    name = "n_registros"
  ) %>%
  arrange(ano, cod_estacao, regra_validade)

write.csv(
  resumo_regras,
  file.path(pasta_saida, "v2_resumo_regras_validade.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# Registros excluidos do valor horario analitico.
registros_nao_elegiveis <- dados_classificados %>%
  filter(!elegivel_analise) %>%
  select(
    id_linha_v1, ano, cod_estacao, no_estacao,
    dh_medicao, dh_arredondado, nu_concentracao,
    st_situacao, cd_flag, ds_flag, no_fonte_dados,
    regra_validade
  ) %>%
  arrange(ano, cod_estacao, dh_arredondado)

write.csv(
  registros_nao_elegiveis,
  file.path(pasta_saida, "v2_registros_nao_elegiveis.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# ------------------------------------------------------------
# 4. CONSTRUIR UMA OBSERVACAO POR ESTACAO-HORA
# ------------------------------------------------------------

# A funcao unique() impede que uma repeticao identica tenha peso
# maior que outro valor distinto no mesmo timestamp.

v2_horaria <- dados_classificados %>%
  group_by(ano, cod_estacao, no_estacao, dh_arredondado) %>%
  summarise(
    data = as.Date(first(dh_arredondado)),
    n_linhas_v1 = n(),
    n_linhas_elegiveis = sum(elegivel_analise),
    n_linhas_nao_elegiveis = sum(!elegivel_analise),
    n_valores_distintos_elegiveis = {
      x <- unique(nu_concentracao[elegivel_analise & !is.na(nu_concentracao)])
      length(x)
    },
    nu_concentracao_horaria = {
      x <- unique(nu_concentracao[elegivel_analise & !is.na(nu_concentracao)])
      if (length(x) == 0) NA_real_ else mean(x)
    },
    nu_concentracao_horaria_mediana = {
      x <- unique(nu_concentracao[elegivel_analise & !is.na(nu_concentracao)])
      if (length(x) == 0) NA_real_ else median(x)
    },
    minimo_elegivel_hora = {
      x <- unique(nu_concentracao[elegivel_analise & !is.na(nu_concentracao)])
      if (length(x) == 0) NA_real_ else min(x)
    },
    maximo_elegivel_hora = {
      x <- unique(nu_concentracao[elegivel_analise & !is.na(nu_concentracao)])
      if (length(x) == 0) NA_real_ else max(x)
    },
    tem_zero_elegivel = any(elegivel_analise & !is.na(nu_concentracao) & nu_concentracao == 0),
    tem_registro_status_in = any(st_situacao == "IN", na.rm = TRUE),
    fontes = paste(sort(unique(no_fonte_dados[!is.na(no_fonte_dados) & no_fonte_dados != ""])), collapse = " | "),
    .groups = "drop"
  ) %>%
  mutate(
    amplitude_elegivel_hora = maximo_elegivel_hora - minimo_elegivel_hora,
    tipo_hora = case_when(
      n_linhas_elegiveis == 0 ~ "SEM_VALOR_ELEGIVEL",
      n_linhas_v1 == 1 & n_valores_distintos_elegiveis == 1 ~ "UNICA",
      n_linhas_v1 > 1 & n_valores_distintos_elegiveis == 1 ~ "DUPLICADA_CONCORDANTE",
      n_valores_distintos_elegiveis > 1 ~ "DUPLICADA_DIVERGENTE_MEDIA_DISTINTOS",
      TRUE ~ "OUTRA"
    ),
    diferenca_media_mediana = nu_concentracao_horaria - nu_concentracao_horaria_mediana
  ) %>%
  arrange(ano, cod_estacao, dh_arredondado)

# Salva a serie analitica completa apenas em RDS.
saveRDS(
  v2_horaria,
  file = "dados_MP10_horario_v2.rds",
  compress = "xz"
)

# ------------------------------------------------------------
# 5. AUDITORIA DA RESOLUCAO DE DUPLICIDADES
# ------------------------------------------------------------

horas_duplicadas <- v2_horaria %>%
  filter(n_linhas_v1 > 1) %>%
  select(
    ano, cod_estacao, no_estacao, dh_arredondado,
    n_linhas_v1, n_linhas_elegiveis,
    n_valores_distintos_elegiveis,
    nu_concentracao_horaria,
    nu_concentracao_horaria_mediana,
    minimo_elegivel_hora, maximo_elegivel_hora,
    amplitude_elegivel_hora, tipo_hora,
    diferenca_media_mediana
  )

write.csv(
  horas_duplicadas,
  file.path(pasta_saida, "v2_horas_duplicadas_resolvidas.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# ------------------------------------------------------------
# 6. RESUMO POR ESTACAO-ANO
# ------------------------------------------------------------

resumo_v2 <- v2_horaria %>%
  group_by(ano, cod_estacao, no_estacao) %>%
  summarise(
    n_horas_total_grade_observada = n(),
    n_horas_com_valor = sum(!is.na(nu_concentracao_horaria)),
    n_horas_sem_valor = sum(is.na(nu_concentracao_horaria)),
    n_horas_unicas = sum(tipo_hora == "UNICA"),
    n_horas_dup_concordante = sum(tipo_hora == "DUPLICADA_CONCORDANTE"),
    n_horas_dup_divergente = sum(tipo_hora == "DUPLICADA_DIVERGENTE_MEDIA_DISTINTOS"),
    n_horas_com_zero_elegivel = sum(tem_zero_elegivel),
    media_horaria_v2 = mean(nu_concentracao_horaria, na.rm = TRUE),
    mediana_horaria_v2 = median(nu_concentracao_horaria, na.rm = TRUE),
    max_abs_diferenca_media_mediana = {
      x <- abs(diferenca_media_mediana[is.finite(diferenca_media_mediana)])
      if (length(x) == 0) NA_real_ else max(x)
    },
    .groups = "drop"
  ) %>%
  arrange(ano, cod_estacao)

write.csv(
  resumo_v2,
  file.path(pasta_saida, "v2_resumo_estacao_ano.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# ------------------------------------------------------------
# 7. TABELA FORMAL DAS REGRAS
# ------------------------------------------------------------

regras_documentadas <- data.frame(
  ordem = 1:8,
  situacao = c(
    "nu_concentracao ausente",
    "st_situacao == IN",
    "st_situacao == VA",
    "serie historica ate 2022 sem st_situacao",
    "concentracao igual a zero",
    "concentracao alta",
    "duplicidade com mesmo valor elegivel",
    "duplicidade com valores elegiveis divergentes"
  ),
  decisao = c(
    "Nao elegivel",
    "Nao elegivel",
    "Elegivel para analise",
    "Elegivel provisoriamente se numerica e nao ausente",
    "Manter; sinalizar",
    "Manter; nao excluir por magnitude",
    "Colapsar para um valor",
    "Media aritmetica dos valores distintos; mediana preservada para sensibilidade"
  ),
  justificativa = c(
    "Sem valor de concentracao para compor a hora",
    "Status de validacao da medicao na fonte",
    "Status de validacao da medicao na fonte",
    "A fonte historica nao traz flag equivalente no conjunto consolidado",
    "Magnitude isolada nao define falha; zeros permanecem auditaveis",
    "Magnitude isolada nao define falha",
    "Evita peso duplicado para repeticao identica",
    "Evita peso extra de copias identicas e produz uma unica observacao horaria reproduzivel"
  ),
  stringsAsFactors = FALSE
)

write.csv(
  regras_documentadas,
  file.path(pasta_saida, "v2_regras_formais.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# ------------------------------------------------------------
# 8. RESUMO FINAL
# ------------------------------------------------------------

resumo_execucao <- data.frame(
  indicador = c(
    "n_linhas_v1",
    "n_linhas_elegiveis",
    "n_linhas_nao_elegiveis",
    "n_horas_v2",
    "n_horas_com_valor_v2",
    "n_horas_sem_valor_v2",
    "n_horas_dup_concordante",
    "n_horas_dup_divergente",
    "n_horas_zero_elegivel"
  ),
  valor = c(
    nrow(dados_classificados),
    sum(dados_classificados$elegivel_analise),
    sum(!dados_classificados$elegivel_analise),
    nrow(v2_horaria),
    sum(!is.na(v2_horaria$nu_concentracao_horaria)),
    sum(is.na(v2_horaria$nu_concentracao_horaria)),
    sum(v2_horaria$tipo_hora == "DUPLICADA_CONCORDANTE"),
    sum(v2_horaria$tipo_hora == "DUPLICADA_DIVERGENTE_MEDIA_DISTINTOS"),
    sum(v2_horaria$tem_zero_elegivel)
  )
)

write.csv(
  resumo_execucao,
  file.path(pasta_saida, "v2_resumo_execucao.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

cat("\n============================================================\n")
cat("ETAPA 3B - REGRAS DE VALIDADE E BASE V2 CONCLUIDAS\n")
cat("============================================================\n\n")
print(resumo_execucao)
cat("\nBase analitica criada em: dados_MP10_horario_v2.rds\n")
cat("Saidas diagnosticas em:", pasta_saida, "\n")
cat("\nATENCAO: a proxima etapa e avaliar completude diaria/mensal/anual.\n")
