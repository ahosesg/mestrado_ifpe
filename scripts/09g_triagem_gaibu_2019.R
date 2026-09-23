# ============================================================
# ETAPA 9G - TRIAGEM ESPACIAL E TEMPORAL DE GAIBU 2019
# Projeto de Mestrado - MP10 / Suape
# ============================================================
#
# OBJETIVO
# Incorporar a referencia anual Gaibu 2019 ao pareamento
# pluviometrico, utilizando coordenada historica documentada e os
# diagnosticos de completude por fonte da Etapa 9D.
#
# COORDENADA HISTORICA DOCUMENTADA
# Estacao Gaibu / PE01:
# S 8° 19' 44.37" / O 34° 57' 22.66"
# Decimal:
# latitude  = -8.328991667
# longitude = -34.956294444
#
# REGRAS
# - distancia geodesica por Haversine;
# - CEMADEN: usa cobertura primaria da 9D (max gap <=60 min),
#   com sensibilidades de 70 e 90 min;
# - INMET automatica: usa a regra horaria da 9D;
# - INMET convencional: permanece secundario;
# - calcula sobreposicao com os dias validos de MP10 em Gaibu 2019;
# - nao escolhe automaticamente a estacao final;
# - SUAPE 2022 permanece como pendencia documental separada.
# ============================================================

if (!requireNamespace("data.table", quietly = TRUE)) {
  stop("Instale o pacote 'data.table' antes de executar.")
}
library(data.table)

# ------------------------------------------------------------
# 1. CAMINHOS
# ------------------------------------------------------------

ARQ_INVENTARIO <- file.path(
  "outputs", "09_meteorologia", "precipitacao",
  "inventario_estacoes_precipitacao.csv"
)

ARQ_COMP_DIA <- file.path(
  "outputs", "09_meteorologia", "precipitacao",
  "completude_por_fonte",
  "completude_diaria_precipitacao_por_fonte.rds"
)

ARQ_MP10 <- file.path(
  "outputs", "04_completude",
  "completude_diaria_mp10.csv"
)

SAIDA <- file.path(
  "outputs", "09_meteorologia", "precipitacao",
  "gaibu_2019"
)

dir.create(SAIDA, recursive = TRUE, showWarnings = FALSE)

for (f in c(ARQ_INVENTARIO, ARQ_COMP_DIA, ARQ_MP10)) {
  if (!file.exists(f)) stop("Arquivo necessario nao encontrado: ", f)
}

# ------------------------------------------------------------
# 2. CONSTANTES E FUNCOES
# ------------------------------------------------------------

LAT_GAIBU <- -8.328991667
LON_GAIBU <- -34.956294444
ANO_ALVO <- 2019L
ESTACAO_HISTORICA <- "Gaibu"

haversine_km <- function(lat1, lon1, lat2, lon2) {
  r <- 6371.0088
  rad <- pi / 180

  p1 <- lat1 * rad
  p2 <- lat2 * rad
  dp <- (lat2 - lat1) * rad
  dl <- (lon2 - lon1) * rad

  a <- sin(dp / 2)^2 +
    cos(p1) * cos(p2) * sin(dl / 2)^2

  2 * r * atan2(sqrt(a), sqrt(1 - a))
}

primeiro_texto <- function(x) {
  z <- unique(na.omit(as.character(x)))
  if (!length(z)) NA_character_ else z[1]
}

# ------------------------------------------------------------
# 3. DOCUMENTAR REFERENCIA ESPACIAL
# ------------------------------------------------------------

referencia_gaibu <- data.table(
  estacao_historica = ESTACAO_HISTORICA,
  codigo_historico = "PE01",
  ano_alvo = ANO_ALVO,
  latitude = LAT_GAIBU,
  longitude = LON_GAIBU,
  coordenada_dms = "S 8°19'44.37\" / O 34°57'22.66\"",
  local_documentado = paste(
    "Escola Professora Maria Thamar Leite da Fonseca,",
    "Rodovia PE-28, km 8,8, Enseada dos Corais,",
    "Cabo de Santo Agostinho - PE"
  ),
  status = "coordenada_historica_documentada"
)

fwrite(
  referencia_gaibu,
  file.path(
    SAIDA,
    "referencia_espacial_gaibu_2019.csv"
  ),
  bom = TRUE
)

pendencia_suape <- data.table(
  estacao_historica = "SUAPE",
  codigo_historico = "PE06",
  ano_alvo = 2022L,
  endereco_documentado = paste(
    "Rodovia PE-60, km 10, s/n, Engenho Massangana,",
    "Complexo Portuario de Suape, Ipojuca - PE"
  ),
  coordenada_publicada = "S 8°36'84.50\" / O 35°3'68.45\"",
  motivo_pendencia = paste(
    "Coordenada publicada apresenta segundos superiores a 60;",
    "nao converter nem corrigir por inferencia."
  ),
  status = "pendente_coordenada_documental_valida"
)

fwrite(
  pendencia_suape,
  file.path(
    SAIDA,
    "pendencia_coordenada_suape_2022.csv"
  ),
  bom = TRUE
)

# ------------------------------------------------------------
# 4. INVENTARIO ESPACIAL
# ------------------------------------------------------------

cat("\nCarregando inventario de precipitacao...\n")

inv <- fread(
  ARQ_INVENTARIO,
  encoding = "UTF-8"
)

inv[
  ,
  `:=`(
    fonte_dados = as.character(fonte_dados),
    cod_estacao = as.character(cod_estacao)
  )
]

inv[
  ,
  `:=`(
    latitude = rowMeans(
      cbind(
        as.numeric(latitude_min),
        as.numeric(latitude_max)
      ),
      na.rm = TRUE
    ),
    longitude = rowMeans(
      cbind(
        as.numeric(longitude_min),
        as.numeric(longitude_max)
      ),
      na.rm = TRUE
    )
  )
]

inv[
  !is.finite(latitude),
  latitude := NA_real_
]

inv[
  !is.finite(longitude),
  longitude := NA_real_
]

inv_coord <- inv[
  !is.na(latitude) &
    !is.na(longitude)
]

inv_coord[
  ,
  distancia_gaibu_km := haversine_km(
    LAT_GAIBU,
    LON_GAIBU,
    latitude,
    longitude
  )
]

inv_coord[
  ,
  faixa_distancia := cut(
    distancia_gaibu_km,
    breaks = c(
      -Inf, 5, 10, 20, 30, 50, Inf
    ),
    labels = c(
      "<=5 km",
      ">5-10 km",
      ">10-20 km",
      ">20-30 km",
      ">30-50 km",
      ">50 km"
    )
  )
]

setorder(
  inv_coord,
  distancia_gaibu_km
)

fwrite(
  inv_coord[
    distancia_gaibu_km <= 50
  ],
  file.path(
    SAIDA,
    "estacoes_precipitacao_ate50km_gaibu.csv"
  ),
  bom = TRUE
)

# ------------------------------------------------------------
# 5. COMPLETUDE 2019 DA ETAPA 9D
# ------------------------------------------------------------

cat("Carregando completude por fonte da Etapa 9D...\n")

comp <- readRDS(ARQ_COMP_DIA)
setDT(comp)

comp[
  ,
  `:=`(
    fonte_dados = as.character(fonte_dados),
    cod_estacao = as.character(cod_estacao),
    ano = as.integer(ano)
  )
]

comp_2019 <- comp[
  ano == ANO_ALVO
]

if (!nrow(comp_2019)) {
  stop("Nao ha dados de completude para 2019 na Etapa 9D.")
}

# Resumo anual de cobertura por estacao.
cobertura_2019 <- comp_2019[
  ,
  .(
    tipo_regra = primeiro_texto(tipo_regra),
    criterio_primario = primeiro_texto(criterio_primario),
    elegivel_pareamento_primario = any(
      elegivel_pareamento_primario %in% TRUE
    ),
    n_dias_calendario = .N,
    n_dias_com_valor = sum(
      n_obs_validas > 0,
      na.rm = TRUE
    ),
    n_dias_primario = sum(
      cobertura_primaria %in% TRUE
    ),
    n_dias_sens_1 = sum(
      cobertura_sens_1 %in% TRUE
    ),
    n_dias_sens_2 = sum(
      cobertura_sens_2 %in% TRUE
    ),
    n_dias_convencional = sum(
      disponibilidade_24h_convencional %in% TRUE
    )
  ),
  by = .(
    fonte_dados,
    cod_estacao
  )
]

cobertura_2019[
  ,
  `:=`(
    prop_dias_primario =
      n_dias_primario / n_dias_calendario,
    prop_dias_sens_1 =
      n_dias_sens_1 / n_dias_calendario,
    prop_dias_sens_2 =
      n_dias_sens_2 / n_dias_calendario
  )
]

# ------------------------------------------------------------
# 6. DIAS VALIDOS DE MP10 EM GAIBU 2019
# ------------------------------------------------------------

cat("Carregando dias validos de MP10 de Gaibu 2019...\n")

mp10 <- fread(
  ARQ_MP10,
  encoding = "UTF-8"
)

mp10[
  ,
  `:=`(
    ano = as.integer(ano),
    data_local = as.IDate(data),
    estacao_mp10 = as.character(no_estacao)
  )
]

mp10_gaibu <- mp10[
  ano == ANO_ALVO &
    toupper(estacao_mp10) == "GAIBU"
]

if (!nrow(mp10_gaibu)) {
  stop("Gaibu 2019 nao foi localizada na base diaria de MP10.")
}

dias_mp10 <- mp10_gaibu[
  dia_valido_mma_16h == TRUE,
  .(
    data_local
  )
]

n_dias_mp10_validos <- nrow(dias_mp10)

# ------------------------------------------------------------
# 7. SOBREPOSICAO MP10 X PRECIPITACAO
# ------------------------------------------------------------

cat("Calculando sobreposicao temporal com Gaibu 2019...\n")

cand50 <- inv_coord[
  distancia_gaibu_km <= 50,
  .(
    fonte_dados,
    cod_estacao,
    nome_estacao,
    municipios,
    tipo_estacao,
    latitude,
    longitude,
    distancia_gaibu_km,
    faixa_distancia
  )
]

# Produto cartesiano entre candidatos e dias validos de MP10.
# merge.data.table() exige uma chave nao vazia; por isso criamos
# uma chave temporaria apenas para realizar o cross join.
cand_dias <- unique(
  cand50[
    ,
    .(
      fonte_dados,
      cod_estacao
    )
  ]
)

cand_dias[
  ,
  chave_tmp := 1L
]

dias_mp10_cross <- copy(
  dias_mp10
)

dias_mp10_cross[
  ,
  chave_tmp := 1L
]

pares_dia <- merge(
  cand_dias,
  dias_mp10_cross,
  by = "chave_tmp",
  allow.cartesian = TRUE
)

pares_dia[
  ,
  chave_tmp := NULL
]

rm(
  cand_dias,
  dias_mp10_cross
)

comp_join <- comp_2019[
  ,
  .(
    fonte_dados,
    cod_estacao,
    data_local,
    n_obs_validas,
    cobertura_primaria,
    cobertura_sens_1,
    cobertura_sens_2,
    disponibilidade_24h_convencional,
    elegivel_pareamento_primario,
    tipo_regra,
    criterio_primario
  )
]

pares_dia <- merge(
  pares_dia,
  comp_join,
  by = c(
    "fonte_dados",
    "cod_estacao",
    "data_local"
  ),
  all.x = TRUE
)

sobreposicao <- pares_dia[
  ,
  .(
    n_dias_mp10_validos = .N,
    n_dias_precip_com_valor = sum(
      n_obs_validas > 0,
      na.rm = TRUE
    ),
    n_overlap_primario = sum(
      cobertura_primaria %in% TRUE
    ),
    n_overlap_sens_1 = sum(
      cobertura_sens_1 %in% TRUE
    ),
    n_overlap_sens_2 = sum(
      cobertura_sens_2 %in% TRUE
    ),
    n_overlap_convencional = sum(
      disponibilidade_24h_convencional %in% TRUE
    ),
    tipo_regra = primeiro_texto(
      tipo_regra
    ),
    criterio_primario = primeiro_texto(
      criterio_primario
    ),
    elegivel_pareamento_primario = any(
      elegivel_pareamento_primario %in% TRUE
    )
  ),
  by = .(
    fonte_dados,
    cod_estacao
  )
]

sobreposicao[
  ,
  `:=`(
    pct_dias_precip_com_valor =
      100 *
        n_dias_precip_com_valor /
        n_dias_mp10_validos,
    pct_overlap_primario =
      100 *
        n_overlap_primario /
        n_dias_mp10_validos,
    pct_overlap_sens_1 =
      100 *
        n_overlap_sens_1 /
        n_dias_mp10_validos,
    pct_overlap_sens_2 =
      100 *
        n_overlap_sens_2 /
        n_dias_mp10_validos,
    pct_overlap_convencional =
      100 *
        n_overlap_convencional /
        n_dias_mp10_validos
  )
]

avaliacao <- merge(
  cand50,
  cobertura_2019,
  by = c(
    "fonte_dados",
    "cod_estacao"
  ),
  all.x = TRUE
)

avaliacao <- merge(
  avaliacao,
  sobreposicao,
  by = c(
    "fonte_dados",
    "cod_estacao"
  ),
  all.x = TRUE,
  suffixes = c(
    "_anual",
    "_mp10"
  )
)

avaliacao[
  ,
  estacao_historica := ESTACAO_HISTORICA
]

avaliacao[
  ,
  ano_alvo := ANO_ALVO
]

setorder(
  avaliacao,
  distancia_gaibu_km,
  -pct_overlap_primario
)

fwrite(
  avaliacao,
  file.path(
    SAIDA,
    "avaliacao_candidatos_gaibu_2019.csv"
  ),
  bom = TRUE
)

# ------------------------------------------------------------
# 8. LISTAS POR DISTANCIA
# ------------------------------------------------------------

for (limite in c(10, 20, 30, 50)) {

  tab <- avaliacao[
    distancia_gaibu_km <= limite
  ]

  fwrite(
    tab,
    file.path(
      SAIDA,
      sprintf(
        "candidatos_gaibu_ate_%dkm.csv",
        limite
      )
    ),
    bom = TRUE
  )
}

lista_curta <- avaliacao[
  distancia_gaibu_km <= 20 &
    elegivel_pareamento_primario_mp10 == TRUE &
    !is.na(
      pct_overlap_primario
    )
][
  order(
    distancia_gaibu_km,
    -pct_overlap_primario
  )
]

fwrite(
  lista_curta,
  file.path(
    SAIDA,
    "lista_curta_gaibu_2019_ate20km.csv"
  ),
  bom = TRUE
)

# ------------------------------------------------------------
# 9. RESUMO
# ------------------------------------------------------------

resumo <- data.table(
  indicador = c(
    "n_dias_mp10_validos_gaibu_2019",
    "n_estacoes_precip_com_coordenada",
    "n_candidatos_ate10km",
    "n_candidatos_ate20km",
    "n_candidatos_ate30km",
    "n_candidatos_ate50km",
    "n_candidatos_primarios_ate20km"
  ),
  valor = c(
    n_dias_mp10_validos,
    nrow(inv_coord),
    nrow(
      avaliacao[
        distancia_gaibu_km <= 10
      ]
    ),
    nrow(
      avaliacao[
        distancia_gaibu_km <= 20
      ]
    ),
    nrow(
      avaliacao[
        distancia_gaibu_km <= 30
      ]
    ),
    nrow(
      avaliacao[
        distancia_gaibu_km <= 50
      ]
    ),
    nrow(lista_curta)
  )
)

fwrite(
  resumo,
  file.path(
    SAIDA,
    "resumo_execucao_gaibu_2019.csv"
  ),
  bom = TRUE
)

cat("\n============================================================\n")
cat("ETAPA 9G - TRIAGEM DE GAIBU 2019 CONCLUIDA\n")
cat("============================================================\n\n")

print(resumo)

cat("\nLista curta Gaibu 2019, ate 20 km:\n")

if (nrow(lista_curta) > 0) {
  print(
    lista_curta[
      ,
      .(
        fonte_dados,
        cod_estacao,
        nome_estacao,
        municipios,
        distancia_gaibu_km,
        tipo_regra = tipo_regra_mp10,
        criterio_primario = criterio_primario_mp10,
        pct_dias_precip_com_valor,
        pct_overlap_primario,
        pct_overlap_sens_1,
        pct_overlap_sens_2
      )
    ]
  )
} else {
  cat("Nenhum candidato primario ate 20 km.\n")
}

cat("\nSaidas em:\n")
cat(SAIDA, "\n")

cat(
  "\nIMPORTANTE:\n",
  "- Gaibu 2019 usa coordenada historica documentada;\n",
  "- nenhuma estacao foi escolhida automaticamente;\n",
  "- SUAPE 2022 permanece pendente por coordenada oficial inconsistente;\n",
  "- a proxima etapa deve construir e comparar as series diarias dos candidatos locais de Gaibu.\n",
  sep = ""
)
