# ============================================================
# ETAPA 9B - TRIAGEM ESPACIAL E TEMPORAL DAS ESTACOES DE PRECIPITACAO
# Projeto de Mestrado - MP10 / Suape
# ============================================================
#
# OBJETIVO
# Reduzir o universo de estacoes de precipitacao consolidado na Etapa 9A
# por meio de criterios espaciais e temporais, sem escolher ainda uma
# estacao meteorologica definitiva para cada estacao de MP10.
#
# PRINCIPIOS
# - proximidade espacial e cobertura temporal sao avaliadas separadamente;
# - nao e criado escore arbitrario com pesos;
# - os registros duplicados nao entram no calculo de cobertura por dia;
# - a coordenada 2025 das estacoes MonitorAr nao e usada nesta triagem,
#   porque a Etapa 2 identificou coordenadas compartilhadas/suspeitas;
# - usa-se a coordenada MonitorAr de 2024 como referencia espacial recente
#   para IPOJUCA, EDCUPE, IFPE e CPRH;
# - Gaibu e SUAPE permanecem pendentes por falta de coordenada documental
#   validada nesta etapa;
# - EDCUPE e usada apenas como referencia espacial recente para CUPE,
#   sem assumir equivalencia cadastral definitiva.
# ============================================================

library(data.table)

# ------------------------------------------------------------
# 1. CAMINHOS
# ------------------------------------------------------------

arq_inventario <- file.path(
  "outputs", "09_meteorologia", "precipitacao",
  "inventario_estacoes_precipitacao.csv"
)

arq_cobertura <- file.path(
  "outputs", "09_meteorologia", "precipitacao",
  "cobertura_precipitacao_estacao_ano.csv"
)

arq_mp10 <- file.path(
  "outputs", "02_estacoes",
  "coordenadas_monitorar_estacao_ano.csv"
)

pasta_saida <- file.path(
  "outputs", "09_meteorologia", "precipitacao", "triagem_estacoes"
)

dir.create(pasta_saida, recursive = TRUE, showWarnings = FALSE)

for (f in c(arq_inventario, arq_cobertura, arq_mp10)) {
  if (!file.exists(f)) stop("Arquivo necessario nao encontrado: ", f)
}

# ------------------------------------------------------------
# 2. FUNCOES
# ------------------------------------------------------------

haversine_km <- function(lat1, lon1, lat2, lon2) {
  R <- 6371.0088
  rad <- pi / 180
  lat1 <- lat1 * rad
  lon1 <- lon1 * rad
  lat2 <- lat2 * rad
  lon2 <- lon2 * rad
  dlat <- lat2 - lat1
  dlon <- lon2 - lon1
  a <- sin(dlat / 2)^2 + cos(lat1) * cos(lat2) * sin(dlon / 2)^2
  2 * R * asin(pmin(1, sqrt(a)))
}

# ------------------------------------------------------------
# 3. LER DADOS
# ------------------------------------------------------------

inv <- fread(arq_inventario, encoding = "UTF-8")
cob <- fread(arq_cobertura, encoding = "UTF-8")
mp10_raw <- fread(arq_mp10, encoding = "UTF-8")

# ------------------------------------------------------------
# 4. REFERENCIAS ESPACIAIS DAS ESTACOES DE MP10
# ------------------------------------------------------------

# Usa apenas 2024, pois as coordenadas 2025 foram sinalizadas na Etapa 2.
mp10 <- mp10_raw[
  ano == 2024 &
    is.finite(latitude_mediana) &
    is.finite(longitude_mediana),
  .(
    cod_estacao_mp10 = as.character(cod_estacao),
    estacao_mp10 = as.character(no_estacao),
    municipio_mp10 = as.character(no_municipio),
    latitude_mp10 = latitude_mediana,
    longitude_mp10 = longitude_mediana,
    ano_coord_referencia = ano
  )
]

mp10[, observacao_coord := fifelse(
  estacao_mp10 == "EDCUPE",
  "Referencia espacial recente; equivalencia com CUPE historica ainda requer confirmacao documental",
  "Coordenada MonitorAr 2024 usada como referencia espacial recente"
)]

fwrite(
  mp10,
  file.path(pasta_saida, "referencias_espaciais_mp10_2024.csv"),
  bom = TRUE
)

# ------------------------------------------------------------
# 5. COORDENADA REPRESENTATIVA DAS ESTACOES DE PRECIPITACAO
# ------------------------------------------------------------

inv[, latitude_ref := rowMeans(cbind(latitude_min, latitude_max), na.rm = TRUE)]
inv[, longitude_ref := rowMeans(cbind(longitude_min, longitude_max), na.rm = TRUE)]

inv[!is.finite(latitude_ref), latitude_ref := NA_real_]
inv[!is.finite(longitude_ref), longitude_ref := NA_real_]

inv[, amplitude_lat := abs(latitude_max - latitude_min)]
inv[, amplitude_lon := abs(longitude_max - longitude_min)]

# Limiar apenas diagnostico: aproximadamente 1 km em latitude por 0,01 grau.
inv[, flag_coordenada_variavel :=
      (!is.na(amplitude_lat) & amplitude_lat > 0.01) |
      (!is.na(amplitude_lon) & amplitude_lon > 0.01)]

# ------------------------------------------------------------
# 6. RESUMO TEMPORAL POR ESTACAO DE PRECIPITACAO
# ------------------------------------------------------------

cob[, ano := as.integer(ano)]
cob[, prop_registros_com_valor := fifelse(
  n_registros > 0,
  n_registros_com_valor / n_registros,
  NA_real_
)]

resumo_temp <- cob[
  , .(
    primeiro_ano = min(ano, na.rm = TRUE),
    ultimo_ano = max(ano, na.rm = TRUE),
    n_anos_com_dados = uniqueN(ano),
    n_anos_12_meses = sum(n_meses_com_registro == 12, na.rm = TRUE),
    n_anos_300_dias = sum(n_datas_com_registro >= 300, na.rm = TRUE),
    n_anos_330_dias = sum(n_datas_com_registro >= 330, na.rm = TRUE),
    media_dias_com_registro = mean(n_datas_com_registro, na.rm = TRUE),
    menor_prop_registros_com_valor = min(prop_registros_com_valor, na.rm = TRUE),
    media_prop_registros_com_valor = mean(prop_registros_com_valor, na.rm = TRUE)
  ),
  by = .(fonte_dados, cod_estacao)
]

# Corrige Inf em casos patologicos.
for (cc in c("menor_prop_registros_com_valor", "media_prop_registros_com_valor")) {
  resumo_temp[!is.finite(get(cc)), (cc) := NA_real_]
}

inv2 <- merge(
  inv,
  resumo_temp,
  by = c("fonte_dados", "cod_estacao"),
  all.x = TRUE
)

# ------------------------------------------------------------
# 7. DISTANCIAS MP10 x PRECIPITACAO
# ------------------------------------------------------------

precip_coord <- inv2[
  is.finite(latitude_ref) & is.finite(longitude_ref)
]

lista_dist <- vector("list", nrow(mp10))

for (i in seq_len(nrow(mp10))) {
  z <- copy(precip_coord)
  z[, `:=`(
    cod_estacao_mp10 = mp10$cod_estacao_mp10[i],
    estacao_mp10 = mp10$estacao_mp10[i],
    municipio_mp10 = mp10$municipio_mp10[i],
    latitude_mp10 = mp10$latitude_mp10[i],
    longitude_mp10 = mp10$longitude_mp10[i]
  )]
  z[, distancia_km := haversine_km(
    latitude_mp10, longitude_mp10,
    latitude_ref, longitude_ref
  )]
  lista_dist[[i]] <- z
}

distancias <- rbindlist(lista_dist, use.names = TRUE, fill = TRUE)

distancias[, faixa_distancia := cut(
  distancia_km,
  breaks = c(-Inf, 5, 10, 20, 30, 50, Inf),
  labels = c("<=5 km", ">5-10 km", ">10-20 km", ">20-30 km", ">30-50 km", ">50 km")
)]

setorder(distancias, estacao_mp10, distancia_km, -n_anos_12_meses)

fwrite(
  distancias,
  file.path(pasta_saida, "distancias_mp10_estacoes_precipitacao.csv"),
  bom = TRUE
)

# Triagem ampla: ate 50 km.
cand50 <- distancias[distancia_km <= 50]
setorder(cand50, estacao_mp10, distancia_km, -n_anos_12_meses, -n_anos_300_dias)

fwrite(
  cand50,
  file.path(pasta_saida, "candidatos_precipitacao_ate_50km.csv"),
  bom = TRUE
)

cand30 <- distancias[distancia_km <= 30]
setorder(cand30, estacao_mp10, distancia_km, -n_anos_12_meses, -n_anos_300_dias)

fwrite(
  cand30,
  file.path(pasta_saida, "candidatos_precipitacao_ate_30km.csv"),
  bom = TRUE
)

# ------------------------------------------------------------
# 8. COBERTURA NOS ANOS-ALVO DAS REFERENCIAS DE MP10
# ------------------------------------------------------------

# Alvos com coordenada espacial operacional disponivel.
# CUPE usa EDCUPE apenas como referencia espacial provisoria.
alvos <- data.table(
  estacao_mp10 = c("IFPE", "IPOJUCA", "IPOJUCA", "EDCUPE"),
  estacao_historica = c("IFPE", "IPOJUCA", "IPOJUCA", "CUPE"),
  ano_alvo = c(2018L, 2021L, 2025L, 2021L),
  nota = c(
    "Referencia anual elegivel IFPE 2018",
    "Referencia anual elegivel IPOJUCA 2021",
    "Referencia anual elegivel IPOJUCA 2025",
    "CUPE 2021; EDCUPE 2024 usada apenas como referencia espacial provisoria"
  )
)

alvos_dist <- merge(
  cand50,
  alvos,
  by = "estacao_mp10",
  allow.cartesian = TRUE
)

cob_alvo <- cob[
  , .(
    fonte_dados,
    cod_estacao,
    ano,
    n_registros,
    n_registros_com_valor,
    n_registros_sem_valor,
    n_datas_com_registro,
    n_meses_com_registro,
    precipitacao_min,
    precipitacao_max
  )
]

alvos_dist <- merge(
  alvos_dist,
  cob_alvo,
  by.x = c("fonte_dados", "cod_estacao", "ano_alvo"),
  by.y = c("fonte_dados", "cod_estacao", "ano"),
  all.x = TRUE
)

alvos_dist[, ano_tem_dados := !is.na(n_registros)]
alvos_dist[, ano_12_meses := !is.na(n_meses_com_registro) & n_meses_com_registro == 12]
alvos_dist[, ano_300_dias := !is.na(n_datas_com_registro) & n_datas_com_registro >= 300]

setorder(
  alvos_dist,
  estacao_historica,
  ano_alvo,
  -ano_12_meses,
  -ano_300_dias,
  distancia_km
)

fwrite(
  alvos_dist,
  file.path(pasta_saida, "candidatos_precipitacao_anos_alvo.csv"),
  bom = TRUE
)

# ------------------------------------------------------------
# 9. TOP 10 POR PROXIMIDADE, SEM DEFINIR 'MELHOR' ESTACAO
# ------------------------------------------------------------

# Este arquivo e apenas uma lista curta para inspecao manual.
# A ordenacao prioriza cobertura no ano-alvo e depois distancia,
# sem combinar criterios em um escore ponderado.

top10 <- alvos_dist[
  ano_tem_dados == TRUE
][
  order(estacao_historica, ano_alvo, -ano_12_meses, -ano_300_dias, distancia_km),
  head(.SD, 10),
  by = .(estacao_historica, ano_alvo)
]

fwrite(
  top10,
  file.path(pasta_saida, "top10_candidatos_por_ano_alvo.csv"),
  bom = TRUE
)

# ------------------------------------------------------------
# 10. PENDENCIAS DE PAREAMENTO
# ------------------------------------------------------------

pendencias <- data.table(
  item = c(
    "Gaibu 2019",
    "SUAPE 2022",
    "CUPE 2021",
    "Coordenadas MonitorAr 2025",
    "Fuso CEMADEN/INMET",
    "Significado temporal valorMedida CEMADEN",
    "Duplicidades CEMADEN"
  ),
  situacao = c(
    "Sem coordenada documental validada no crosswalk",
    "Sem coordenada documental validada no crosswalk",
    "EDCUPE 2024 usada apenas como referencia espacial provisoria; confirmar equivalencia",
    "Nao usadas nesta triagem por coordenadas compartilhadas/suspeitas",
    "Confirmar documentalmente antes da integracao definitiva",
    "Confirmar intervalo de acumulacao antes de agregar chuva diaria",
    "Diagnosticar origem; cobertura por dia nao depende da contagem duplicada"
  )
)

fwrite(
  pendencias,
  file.path(pasta_saida, "pendencias_pareamento_precipitacao.csv"),
  bom = TRUE
)

# ------------------------------------------------------------
# 11. RESUMO
# ------------------------------------------------------------

resumo <- data.table(
  indicador = c(
    "n_referencias_espaciais_mp10",
    "n_estacoes_precipitacao_com_coordenada",
    "n_pares_mp10_precipitacao_total",
    "n_pares_ate_50km",
    "n_pares_ate_30km",
    "n_estacoes_precipitacao_ate30km_unicas",
    "n_estacoes_precipitacao_coord_variavel_ate30km",
    "n_alvos_estacao_ano_avaliados"
  ),
  valor = c(
    nrow(mp10),
    nrow(precip_coord),
    nrow(distancias),
    nrow(cand50),
    nrow(cand30),
    uniqueN(cand30[, paste(fonte_dados, cod_estacao, sep = "::")]),
    uniqueN(cand30[flag_coordenada_variavel == TRUE, paste(fonte_dados, cod_estacao, sep = "::")]),
    nrow(alvos)
  )
)

fwrite(
  resumo,
  file.path(pasta_saida, "resumo_execucao_triagem_precipitacao.csv"),
  bom = TRUE
)

cat("\n============================================================\n")
cat("ETAPA 9B - TRIAGEM DE ESTACOES DE PRECIPITACAO CONCLUIDA\n")
cat("============================================================\n\n")
print(resumo)
cat("\nSaidas em:", pasta_saida, "\n")
cat("\nIMPORTANTE: esta etapa faz triagem; nao define ainda o pareamento final.\n")
