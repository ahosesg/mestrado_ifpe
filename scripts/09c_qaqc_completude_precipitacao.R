# ETAPA 9C - QA/QC E COMPLETUDE EFETIVA DA PRECIPITACAO
# Fontes: CEMADEN e INMET | Periodo: 2017-2025
#
# Regras:
# - remove apenas duplicatas exatas;
# - preserva conflitos no mesmo timestamp e nao calcula media;
# - nao exclui valores positivos extremos por magnitude;
# - infere a cadencia efetiva por estacao-ano;
# - calcula completude diaria em 75%, 80%, 90% e 100%;
# - 90% e limiar operacional de triagem, nao criterio normativo;
# - nao agrega/soma precipitacao diaria nesta etapa.

if (!requireNamespace("data.table", quietly = TRUE)) {
  stop("Instale o pacote data.table antes de executar.")
}
library(data.table)

ARQ_PRECIP <- file.path(
  "outputs", "09_meteorologia", "precipitacao",
  "precipitacao_2017_2025_consolidada.rds"
)
ARQ_CAND <- file.path(
  "outputs", "09_meteorologia", "precipitacao",
  "triagem_estacoes", "candidatos_precipitacao_anos_alvo.csv"
)
ARQ_MP10 <- file.path(
  "outputs", "04_completude", "completude_diaria_mp10.csv"
)
SAIDA <- file.path(
  "outputs", "09_meteorologia", "precipitacao",
  "qaqc_completude"
)
dir.create(SAIDA, recursive = TRUE, showWarnings = FALSE)

for (f in c(ARQ_PRECIP, ARQ_CAND, ARQ_MP10)) {
  if (!file.exists(f)) stop("Arquivo nao encontrado: ", f)
}

moda_int <- function(x) {
  x <- x[!is.na(x)]
  if (!length(x)) return(NA_integer_)
  tb <- table(x)
  max(as.integer(names(tb)[tb == max(tb)]))
}
prop_true <- function(x) {
  if (!length(x) || all(is.na(x))) return(NA_real_)
  mean(x, na.rm = TRUE)
}
qseg <- function(x, p) {
  x <- x[is.finite(x)]
  if (!length(x)) return(NA_real_)
  as.numeric(quantile(x, p, na.rm = TRUE, names = FALSE))
}
maior_lacuna <- function(x) {
  if (!length(x) || all(is.na(x))) return(NA_integer_)
  x <- ifelse(is.na(x), FALSE, x)
  r <- rle(!x)
  if (!any(r$values)) return(0L)
  as.integer(max(r$lengths[r$values]))
}

# -------------------------------------------------------------------
# 1. REGRAS DOCUMENTADAS
# -------------------------------------------------------------------

regras <- data.table(
  item = c(
    "duplicata_exata", "conflito_timestamp", "ausente", "negativo",
    "extremo_positivo", "cadencia", "completude_diaria",
    "limiares", "agregacao_diaria", "fuso", "pareamento_final"
  ),
  regra = c(
    "Mesma fonte+estacao+timestamp+valor: manter uma linha.",
    "Mesmo timestamp com valores distintos: preservar e sinalizar.",
    "Nao conta como observacao valida.",
    "Sinalizar como invalido para precipitacao e nao usar na completude.",
    "Preservar e diagnosticar; nao excluir automaticamente.",
    "Inferida por estacao-ano a partir do intervalo modal entre valores validos.",
    "Observacoes validas unicas / observacoes esperadas pela cadencia.",
    "75, 80, 90 e 100% usados como sensibilidade; 90% apenas para triagem.",
    "Nao realizada nesta etapa.",
    "Confirmacao documental permanece pendente.",
    "Definir apenas apos QA/QC, sobreposicao com MP10 e avaliacao espacial."
  )
)
fwrite(regras, file.path(SAIDA, "regras_qaqc_completude_precipitacao.csv"), bom = TRUE)

# -------------------------------------------------------------------
# 2. CARREGAR E REDUZIR BASE
# -------------------------------------------------------------------

cat("\nCarregando base consolidada...\n")
p <- readRDS(ARQ_PRECIP)
setDT(p)

cols <- c(
  "fonte_dados", "cod_estacao", "nome_estacao", "tipo_estacao",
  "periodicidade_fonte", "datahora_local", "data_local", "ano",
  "mes", "precipitacao_mm", "arquivo_origem"
)
falt <- setdiff(cols, names(p))
if (length(falt)) stop("Colunas ausentes: ", paste(falt, collapse = ", "))

p <- p[, ..cols]
p[, `:=`(
  fonte_dados = as.character(fonte_dados),
  cod_estacao = as.character(cod_estacao),
  nome_estacao = as.character(nome_estacao),
  ano = as.integer(ano),
  mes = as.integer(mes),
  precipitacao_mm = as.numeric(precipitacao_mm)
)]
if (!inherits(p$data_local, "IDate")) p[, data_local := as.IDate(data_local)]

n_entrada <- nrow(p)

# -------------------------------------------------------------------
# 3. DUPLICATAS EXATAS
# -------------------------------------------------------------------

cat("Removendo apenas duplicatas exatas...\n")
setorder(p, fonte_dados, cod_estacao, ano, datahora_local, precipitacao_mm, arquivo_origem)

chave_dup <- c("fonte_dados", "cod_estacao", "datahora_local", "precipitacao_mm")
p[, dup_exata := duplicated(p, by = chave_dup)]

diag_dup <- p[, .(
  n_antes = .N,
  n_duplicatas_exatas_removiveis = sum(dup_exata),
  n_depois = .N - sum(dup_exata),
  n_arquivos = uniqueN(arquivo_origem)
), by = .(fonte_dados, cod_estacao, ano)][
  n_duplicatas_exatas_removiveis > 0
]

fwrite(
  diag_dup,
  file.path(SAIDA, "diagnostico_duplicatas_exatas_estacao_ano.csv"),
  bom = TRUE
)

n_dup_removidas <- sum(p$dup_exata)
p <- p[dup_exata == FALSE]
p[, dup_exata := NULL]
gc()

# -------------------------------------------------------------------
# 4. CONFLITOS NO MESMO TIMESTAMP
# -------------------------------------------------------------------

cat("Diagnosticando conflitos no mesmo timestamp...\n")
conf <- p[, .(
  n_linhas = .N,
  n_valores_distintos = uniqueN(precipitacao_mm, na.rm = FALSE)
), by = .(fonte_dados, cod_estacao, datahora_local)][
  n_linhas > 1 & n_valores_distintos > 1
]

p[, conflito_timestamp := FALSE]
if (nrow(conf)) {
  p[conf, on = .(fonte_dados, cod_estacao, datahora_local),
    conflito_timestamp := TRUE]

  conf_det <- p[conflito_timestamp == TRUE, .(
    nome_estacao = paste(sort(unique(na.omit(nome_estacao))), collapse = " | "),
    valores_mm = paste(sort(unique(precipitacao_mm)), collapse = " | "),
    arquivos = paste(sort(unique(arquivo_origem)), collapse = " | ")
  ), by = .(fonte_dados, cod_estacao, datahora_local)]

  conf <- merge(
    conf, conf_det,
    by = c("fonte_dados", "cod_estacao", "datahora_local"),
    all.x = TRUE
  )
}
fwrite(
  conf,
  file.path(SAIDA, "diagnostico_conflitos_timestamp_precipitacao.csv"),
  bom = TRUE
)

# -------------------------------------------------------------------
# 5. FLAGS DE VALIDADE E EXTREMOS
# -------------------------------------------------------------------

p[, `:=`(
  valor_ausente = is.na(precipitacao_mm),
  valor_negativo = !is.na(precipitacao_mm) & precipitacao_mm < 0
)]
p[, valor_valido := !valor_ausente & !valor_negativo & !conflito_timestamp]

diag_ext <- p[valor_valido == TRUE, .(
  n = .N,
  minimo = min(precipitacao_mm),
  p50 = qseg(precipitacao_mm, .50),
  p95 = qseg(precipitacao_mm, .95),
  p99 = qseg(precipitacao_mm, .99),
  p999 = qseg(precipitacao_mm, .999),
  maximo = max(precipitacao_mm)
), by = .(fonte_dados, cod_estacao, ano)]

fwrite(
  diag_ext,
  file.path(SAIDA, "diagnostico_extremos_precipitacao_estacao_ano.csv"),
  bom = TRUE
)

maiores <- p[valor_valido == TRUE][order(-precipitacao_mm)][
  1:min(.N, 250),
  .(fonte_dados, cod_estacao, nome_estacao, ano, datahora_local,
    precipitacao_mm, arquivo_origem)
]
fwrite(maiores, file.path(SAIDA, "maiores_valores_precipitacao.csv"), bom = TRUE)

# -------------------------------------------------------------------
# 6. METADADOS ENXUTOS E CONTAGEM DIARIA
# -------------------------------------------------------------------

# Guarda metadados uma unica vez por estacao-ano e libera colunas
# repetitivas antes das operacoes mais pesadas, reduzindo uso de memoria.
meta_p <- p[, .(
  nome_estacao = {
    z <- unique(na.omit(nome_estacao)); if (!length(z)) NA_character_ else z[1]
  },
  tipo_estacao = {
    z <- unique(na.omit(tipo_estacao)); if (!length(z)) NA_character_ else z[1]
  },
  periodicidade_fonte = {
    z <- unique(na.omit(periodicidade_fonte)); if (!length(z)) NA_character_ else z[1]
  }
), by = .(fonte_dados, cod_estacao, ano)]

p[, c(
  "nome_estacao", "tipo_estacao", "periodicidade_fonte",
  "arquivo_origem", "mes"
) := NULL]
gc()

cat("Calculando observacoes validas por dia...\n")
obs_dia <- p[, .(
  n_linhas = .N,
  n_validas = sum(valor_valido),
  n_ausentes = sum(valor_ausente),
  n_negativos = sum(valor_negativo),
  n_conflitantes = sum(conflito_timestamp)
), by = .(fonte_dados, cod_estacao, ano, data_local)]

# -------------------------------------------------------------------
# 7. CADENCIA EFETIVA
# -------------------------------------------------------------------

cat("Inferindo cadencia por estacao-ano...\n")
tv <- p[valor_valido == TRUE,
        .(fonte_dados, cod_estacao, ano, datahora_local)]
setorder(tv, fonte_dados, cod_estacao, ano, datahora_local)

tv[, delta_min := as.numeric(
  difftime(datahora_local, shift(datahora_local), units = "mins")
), by = .(fonte_dados, cod_estacao, ano)]

fd <- tv[
  is.finite(delta_min) & delta_min > 0 & delta_min <= 1440,
  .N,
  by = .(fonte_dados, cod_estacao, ano, delta_min = round(delta_min, 3))
]
setorder(fd, fonte_dados, cod_estacao, ano, -N, delta_min)

im <- fd[, .SD[1], by = .(fonte_dados, cod_estacao, ano)][, .(
  fonte_dados, cod_estacao, ano,
  intervalo_modal_min = delta_min,
  n_intervalos_modais = N
)]

cd <- obs_dia[n_validas > 0, .(
  n_dias_com_valor = .N,
  moda_n_validas = moda_int(n_validas),
  p50_n_validas = qseg(n_validas, .50),
  p90_n_validas = qseg(n_validas, .90),
  p95_n_validas = qseg(n_validas, .95),
  max_n_validas = max(n_validas)
), by = .(fonte_dados, cod_estacao, ano)]

cad <- merge(
  meta_p, im,
  by = c("fonte_dados", "cod_estacao", "ano"),
  all.x = TRUE
)
cad <- merge(cad, cd, by = c("fonte_dados", "cod_estacao", "ano"), all.x = TRUE)

cad[, n_esperado_intervalo := fifelse(
  !is.na(intervalo_modal_min) & intervalo_modal_min > 0,
  as.integer(round(1440 / intervalo_modal_min)),
  NA_integer_
)]
cad[, erro_fechamento_min := fifelse(
  !is.na(n_esperado_intervalo),
  abs(n_esperado_intervalo * intervalo_modal_min - 1440),
  NA_real_
)]
cad[, intervalo_fecha_dia := !is.na(erro_fechamento_min) & erro_fechamento_min <= 2]

cad[, n_esperado_dia := fifelse(
  intervalo_fecha_dia & n_esperado_intervalo >= 1,
  n_esperado_intervalo,
  as.integer(round(p95_n_validas))
)]
cad[n_esperado_dia < 1, n_esperado_dia := NA_integer_]

cad[, razao_p95_esperado := fifelse(
  !is.na(n_esperado_dia) & n_esperado_dia > 0,
  p95_n_validas / n_esperado_dia,
  NA_real_
)]
cad[, cadencia_inconsistente := is.na(n_esperado_dia) |
      (!is.na(razao_p95_esperado) &
         (razao_p95_esperado < .75 | razao_p95_esperado > 1.25))]

cad[, confianca_cadencia := fifelse(
  is.na(n_esperado_dia), "NAO_INFERIDA",
  fifelse(!cadencia_inconsistente & n_dias_com_valor >= 30, "ALTA",
          fifelse(!cadencia_inconsistente & n_dias_com_valor >= 10,
                  "MEDIA", "BAIXA"))
)]

setorder(cad, fonte_dados, cod_estacao, ano)
fwrite(cad, file.path(SAIDA, "cadencia_inferida_estacao_ano.csv"), bom = TRUE)
fwrite(
  cad[cadencia_inconsistente == TRUE |
        confianca_cadencia %in% c("BAIXA", "NAO_INFERIDA")],
  file.path(SAIDA, "diagnostico_cadencia_inconsistente.csv"),
  bom = TRUE
)

rm(tv, fd, im, cd)
gc()

# -------------------------------------------------------------------
# 8. GRADE DIARIA COMPLETA
# -------------------------------------------------------------------

cat("Construindo grade diaria completa...\n")
ea <- cad[, .(
  fonte_dados, cod_estacao, ano, nome_estacao, tipo_estacao,
  periodicidade_fonte, intervalo_modal_min, n_esperado_dia,
  confianca_cadencia, cadencia_inconsistente
)]

cal <- ea[, .(
  data_local = seq(
    as.IDate(sprintf("%04d-01-01", ano)),
    as.IDate(sprintf("%04d-12-31", ano)),
    by = "day"
  )
), by = .(
  fonte_dados, cod_estacao, ano, nome_estacao, tipo_estacao,
  periodicidade_fonte, intervalo_modal_min, n_esperado_dia,
  confianca_cadencia, cadencia_inconsistente
)]

dia <- merge(
  cal,
  obs_dia[, .(fonte_dados, cod_estacao, ano, data_local,
              n_linhas, n_validas, n_ausentes, n_negativos, n_conflitantes)],
  by = c("fonte_dados", "cod_estacao", "ano", "data_local"),
  all.x = TRUE
)

for (cc in c("n_linhas", "n_validas", "n_ausentes", "n_negativos", "n_conflitantes")) {
  set(dia, which(is.na(dia[[cc]])), cc, 0L)
}

dia[, completude_bruta := fifelse(
  !is.na(n_esperado_dia) & n_esperado_dia > 0,
  n_validas / n_esperado_dia,
  NA_real_
)]
dia[, completude := fifelse(!is.na(completude_bruta),
                            pmin(1, completude_bruta), NA_real_)]
dia[, obs_acima_esperado := !is.na(completude_bruta) & completude_bruta > 1.05]
dia[, algum_valor := n_validas > 0]
dia[, cobertura_75 := fifelse(!is.na(completude), completude >= .75, NA)]
dia[, cobertura_80 := fifelse(!is.na(completude), completude >= .80, NA)]
dia[, cobertura_90 := fifelse(!is.na(completude), completude >= .90, NA)]
dia[, cobertura_100 := fifelse(!is.na(completude), completude >= 1, NA)]
dia[, mes := as.integer(format(data_local, "%m"))]

# -------------------------------------------------------------------
# 9. COMPLETUDE MENSAL E ANUAL
# -------------------------------------------------------------------

cat("Resumindo completude mensal e anual...\n")
mensal <- dia[, .(
  n_dias_calendario = .N,
  n_dias_algum_valor = sum(algum_valor),
  n_dias_75 = sum(cobertura_75, na.rm = TRUE),
  n_dias_80 = sum(cobertura_80, na.rm = TRUE),
  n_dias_90 = sum(cobertura_90, na.rm = TRUE),
  n_dias_100 = sum(cobertura_100, na.rm = TRUE),
  prop_algum_valor = mean(algum_valor),
  prop_75 = prop_true(cobertura_75),
  prop_80 = prop_true(cobertura_80),
  prop_90 = prop_true(cobertura_90),
  prop_100 = prop_true(cobertura_100),
  n_dias_acima_esperado = sum(obs_acima_esperado)
), by = .(
  fonte_dados, cod_estacao, nome_estacao, ano, mes,
  n_esperado_dia, confianca_cadencia, cadencia_inconsistente
)]
mensal[, mes_80pct_dias_90 := fifelse(!is.na(prop_90), prop_90 >= .80, NA)]

fwrite(
  mensal,
  file.path(SAIDA, "completude_mensal_precipitacao_qaqc.csv"),
  bom = TRUE
)

anual <- dia[, .(
  n_dias_calendario = .N,
  n_dias_algum_valor = sum(algum_valor),
  n_dias_75 = sum(cobertura_75, na.rm = TRUE),
  n_dias_80 = sum(cobertura_80, na.rm = TRUE),
  n_dias_90 = sum(cobertura_90, na.rm = TRUE),
  n_dias_100 = sum(cobertura_100, na.rm = TRUE),
  prop_algum_valor = mean(algum_valor),
  prop_75 = prop_true(cobertura_75),
  prop_80 = prop_true(cobertura_80),
  prop_90 = prop_true(cobertura_90),
  prop_100 = prop_true(cobertura_100),
  maior_lacuna_sem_90 = maior_lacuna(cobertura_90),
  n_dias_acima_esperado = sum(obs_acima_esperado)
), by = .(
  fonte_dados, cod_estacao, nome_estacao, tipo_estacao,
  periodicidade_fonte, ano, intervalo_modal_min, n_esperado_dia,
  confianca_cadencia, cadencia_inconsistente
)]

m_ano <- mensal[, .(
  n_meses_algum_valor = sum(n_dias_algum_valor > 0),
  n_meses_80pct_dias_90 = sum(mes_80pct_dias_90, na.rm = TRUE)
), by = .(fonte_dados, cod_estacao, ano)]

anual <- merge(anual, m_ano,
               by = c("fonte_dados", "cod_estacao", "ano"),
               all.x = TRUE)
setorder(anual, fonte_dados, cod_estacao, ano)

fwrite(
  anual,
  file.path(SAIDA, "completude_anual_precipitacao_qaqc.csv"),
  bom = TRUE
)

# -------------------------------------------------------------------
# 10. SOBREPOSICAO COM DIAS VALIDOS DE MP10
# -------------------------------------------------------------------

cat("Calculando sobreposicao com dias validos de MP10...\n")
cand <- fread(ARQ_CAND, encoding = "UTF-8")
mp10 <- fread(ARQ_MP10, encoding = "UTF-8")
mp10[, `:=`(ano = as.integer(ano), data_local = as.IDate(data))]

mpv <- mp10[dia_valido_mma_16h == TRUE, .(
  estacao_historica = as.character(no_estacao),
  ano_alvo = ano,
  data_local
)]

# Elimina repeticoes da 9B decorrentes de variacao de nome da mesma estacao.
cid <- cand[distancia_km <= 50, .(
  nome_estacao_9b = {
    z <- unique(na.omit(nome_estacao)); if (!length(z)) NA_character_ else z[1]
  },
  estacao_mp10 = {
    z <- unique(na.omit(estacao_mp10)); if (!length(z)) NA_character_ else z[1]
  },
  municipio_mp10 = {
    z <- unique(na.omit(municipio_mp10)); if (!length(z)) NA_character_ else z[1]
  },
  distancia_km = min(distancia_km, na.rm = TRUE),
  nota = {
    z <- unique(na.omit(nota)); if (!length(z)) NA_character_ else z[1]
  }
), by = .(estacao_historica, ano_alvo, fonte_dados, cod_estacao)]

aa <- anual[, .(
  fonte_dados, cod_estacao, ano_alvo = ano,
  nome_estacao_qaqc = nome_estacao,
  intervalo_modal_min, n_esperado_dia, confianca_cadencia,
  cadencia_inconsistente,
  n_dias_algum_valor_ano = n_dias_algum_valor,
  n_dias_90_ano = n_dias_90,
  prop_90_ano = prop_90,
  n_meses_80pct_dias_90,
  maior_lacuna_sem_90
)]

cq <- merge(cid, aa,
            by = c("fonte_dados", "cod_estacao", "ano_alvo"),
            all.x = TRUE)

pd <- merge(cid, mpv,
            by = c("estacao_historica", "ano_alvo"),
            allow.cartesian = TRUE)

dj <- dia[, .(
  fonte_dados, cod_estacao, ano_alvo = ano, data_local,
  algum_valor, cobertura_75, cobertura_80, cobertura_90, cobertura_100
)]

pd <- merge(
  pd, dj,
  by = c("fonte_dados", "cod_estacao", "ano_alvo", "data_local"),
  all.x = TRUE
)

ov <- pd[, .(
  n_dias_mp10_validos = .N,
  n_overlap_algum = sum(algum_valor %in% TRUE),
  n_overlap_75 = sum(cobertura_75 %in% TRUE),
  n_overlap_80 = sum(cobertura_80 %in% TRUE),
  n_overlap_90 = sum(cobertura_90 %in% TRUE),
  n_overlap_100 = sum(cobertura_100 %in% TRUE)
), by = .(estacao_historica, ano_alvo, fonte_dados, cod_estacao)]

ov[, `:=`(
  pct_overlap_algum = 100 * n_overlap_algum / n_dias_mp10_validos,
  pct_overlap_75 = 100 * n_overlap_75 / n_dias_mp10_validos,
  pct_overlap_80 = 100 * n_overlap_80 / n_dias_mp10_validos,
  pct_overlap_90 = 100 * n_overlap_90 / n_dias_mp10_validos,
  pct_overlap_100 = 100 * n_overlap_100 / n_dias_mp10_validos
)]

cq <- merge(
  cq, ov,
  by = c("estacao_historica", "ano_alvo", "fonte_dados", "cod_estacao"),
  all.x = TRUE
)

cq[, candidato_com_valor_no_ano :=
     !is.na(n_dias_algum_valor_ano) & n_dias_algum_valor_ano > 0]

setorder(cq, estacao_historica, ano_alvo,
         -pct_overlap_90, -prop_90_ano, distancia_km)

fwrite(
  cq,
  file.path(SAIDA, "candidatos_qaqc_anos_alvo.csv"),
  bom = TRUE
)

top10 <- cq[
  candidato_com_valor_no_ano == TRUE & !is.na(pct_overlap_90)
][
  order(estacao_historica, ano_alvo,
        -pct_overlap_90, -prop_90_ano, distancia_km),
  head(.SD, 10),
  by = .(estacao_historica, ano_alvo)
]

fwrite(
  top10,
  file.path(SAIDA, "top10_candidatos_qaqc_por_ano_alvo.csv"),
  bom = TRUE
)

# -------------------------------------------------------------------
# 11. BASES GRANDES LOCAIS
# -------------------------------------------------------------------

saveRDS(
  p,
  file.path(SAIDA, "precipitacao_2017_2025_qaqc_v2.rds"),
  compress = "gzip"
)
saveRDS(
  dia,
  file.path(SAIDA, "completude_diaria_precipitacao_qaqc.rds"),
  compress = "gzip"
)

# -------------------------------------------------------------------
# 12. RESUMO
# -------------------------------------------------------------------

resumo <- data.table(
  indicador = c(
    "n_linhas_entrada",
    "n_duplicatas_exatas_removidas",
    "n_linhas_apos_dedup",
    "n_grupos_timestamp_conflitantes",
    "n_linhas_conflitantes",
    "n_valores_ausentes",
    "n_valores_negativos",
    "n_valores_validos",
    "n_estacao_ano",
    "n_cadencia_alta",
    "n_cadencia_media",
    "n_cadencia_baixa",
    "n_cadencia_nao_inferida",
    "n_cadencia_inconsistente",
    "n_candidatos_unicos_ate50km_anos_alvo",
    "n_candidatos_com_valor_no_ano",
    "n_alvos_mp10_com_sobreposicao"
  ),
  valor = c(
    n_entrada,
    n_dup_removidas,
    nrow(p),
    nrow(conf),
    sum(p$conflito_timestamp),
    sum(p$valor_ausente),
    sum(p$valor_negativo),
    sum(p$valor_valido),
    nrow(cad),
    sum(cad$confianca_cadencia == "ALTA"),
    sum(cad$confianca_cadencia == "MEDIA"),
    sum(cad$confianca_cadencia == "BAIXA"),
    sum(cad$confianca_cadencia == "NAO_INFERIDA"),
    sum(cad$cadencia_inconsistente),
    nrow(cq),
    sum(cq$candidato_com_valor_no_ano),
    uniqueN(cq[!is.na(pct_overlap_90),
               paste(estacao_historica, ano_alvo, sep = "::")])
  )
)

fwrite(
  resumo,
  file.path(SAIDA, "resumo_execucao_qaqc_precipitacao.csv"),
  bom = TRUE
)

cat("\n============================================================\n")
cat("ETAPA 9C - QA/QC E COMPLETUDE DE PRECIPITACAO CONCLUIDA\n")
cat("============================================================\n\n")
print(resumo)

cat("\nTop candidatos apos QA/QC:\n")
print(
  top10[, .(
    estacao_historica, ano_alvo, fonte_dados, cod_estacao,
    nome_estacao_qaqc, distancia_km, n_esperado_dia,
    confianca_cadencia, prop_90_ano,
    n_dias_mp10_validos, pct_overlap_90
  )]
)

cat("\nSaidas em:", SAIDA, "\n")
cat("\nIMPORTANTE:\n")
cat("- a chuva ainda NAO foi agregada por dia;\n")
cat("- extremos positivos foram preservados;\n")
cat("- conflitos nao foram resolvidos por media;\n")
cat("- 90% e apenas limiar operacional de triagem;\n")
cat("- fuso e significado temporal das medidas ainda devem ser confirmados.\n")
