# ============================================================
# ETAPA 9D - COMPLETUDE DE PRECIPITACAO ESPECIFICA POR FONTE
# Projeto de Mestrado - MP10 / Suape
# ============================================================
#
# MOTIVACAO
# A Etapa 9C mostrou que nao existe uma unica cadencia fixa para
# todos os dados de precipitacao. Para o CEMADEN, a transmissao
# depende da ocorrencia de chuva; para o INMET, automaticas e
# convencionais possuem logicas distintas.
#
# REGRAS OPERACIONAIS
#
# CEMADEN
# - durante chuva: transmissao pode ocorrer a cada 10 min;
# - sem chuva: transmissao de 0 mm pode ocorrer a cada 60 min;
# - portanto, NAO se usa "24 observacoes esperadas/dia";
# - cobertura temporal primaria: nenhum intervalo entre registros
#   validos consecutivos superior a 60 min ao longo do dia;
# - sensibilidades: 70 e 90 min.
#
# INMET AUTOMATICA
# - observacoes horarias;
# - precipitacao horaria = total ocorrido na ultima hora;
# - cobertura primaria: 24 valores horarios validos no dia;
# - sensibilidades: >=20 e >=18 valores/dia.
#
# INMET CONVENCIONAL
# - a precipitacao informada corresponde a acumulado de 24 h;
# - nesta etapa e inventariada a disponibilidade diaria;
# - nao entra na classificacao primaria antes de confirmar o
#   horario de referencia do acumulado e o alinhamento com MP10.
#
# A ETAPA 9D NAO:
# - exclui extremos positivos por magnitude;
# - resolve os conflitos preservados na 9C por media;
# - escolhe automaticamente uma estacao "vencedora";
# - agrega ainda a precipitacao diaria final.
# ============================================================

if (!requireNamespace("data.table", quietly = TRUE)) {
  stop("Instale o pacote 'data.table' antes de executar.")
}

library(data.table)

# ------------------------------------------------------------
# 1. CAMINHOS
# ------------------------------------------------------------

ARQ_QAQC <- file.path(
  "outputs", "09_meteorologia", "precipitacao",
  "qaqc_completude",
  "precipitacao_2017_2025_qaqc_v2.rds"
)

ARQ_INVENTARIO <- file.path(
  "outputs", "09_meteorologia", "precipitacao",
  "inventario_estacoes_precipitacao.csv"
)

ARQ_CANDIDATOS <- file.path(
  "outputs", "09_meteorologia", "precipitacao",
  "triagem_estacoes",
  "candidatos_precipitacao_anos_alvo.csv"
)

ARQ_MP10 <- file.path(
  "outputs", "04_completude",
  "completude_diaria_mp10.csv"
)

PASTA_SAIDA <- file.path(
  "outputs", "09_meteorologia", "precipitacao",
  "completude_por_fonte"
)

ARQ_DIARIO_RDS <- file.path(
  PASTA_SAIDA,
  "completude_diaria_precipitacao_por_fonte.rds"
)

dir.create(PASTA_SAIDA, recursive = TRUE, showWarnings = FALSE)

for (f in c(
  ARQ_QAQC,
  ARQ_INVENTARIO,
  ARQ_CANDIDATOS,
  ARQ_MP10
)) {
  if (!file.exists(f)) {
    stop("Arquivo necessario nao encontrado: ", f)
  }
}

# ------------------------------------------------------------
# 2. FUNCOES
# ------------------------------------------------------------

prop_true <- function(x) {
  if (length(x) == 0 || all(is.na(x))) return(NA_real_)
  mean(x, na.rm = TRUE)
}

maior_lacuna_false <- function(x) {
  if (length(x) == 0 || all(is.na(x))) return(NA_integer_)
  x2 <- ifelse(is.na(x), FALSE, x)
  rr <- rle(!x2)
  if (!any(rr$values)) return(0L)
  as.integer(max(rr$lengths[rr$values]))
}

primeiro_texto <- function(x) {
  z <- unique(na.omit(as.character(x)))
  if (length(z) == 0) NA_character_ else z[1]
}

# ------------------------------------------------------------
# 3. DOCUMENTAR AS REGRAS
# ------------------------------------------------------------

regras <- data.table(
  fonte_tipo = c(
    "CEMADEN",
    "CEMADEN",
    "CEMADEN",
    "INMET_AUTOMATICA",
    "INMET_AUTOMATICA",
    "INMET_CONVENCIONAL",
    "TODAS"
  ),
  componente = c(
    "cadencia",
    "cobertura_primaria",
    "sensibilidade",
    "cobertura_primaria",
    "sensibilidade",
    "disponibilidade",
    "extremos"
  ),
  regra = c(
    "Cadencia variavel: em chuva pode haver transmissao a cada 10 min; sem chuva pode haver transmissao horaria.",
    "Dia com continuidade temporal se o maior intervalo entre registros validos, incluindo as fronteiras do dia, for <=60 min.",
    "Recalcular tambem com limites de 70 e 90 min.",
    "Dia completo quando existem 24 valores horarios validos de precipitacao.",
    "Avaliar tambem >=20 e >=18 valores horarios validos por dia.",
    "Registrar dias com valor de precipitacao 24 h, mas nao usar no pareamento primario ate confirmar horario de referencia do acumulado.",
    "Valores positivos extremos permanecem preservados; a 9D nao os exclui por magnitude."
  )
)

fwrite(
  regras,
  file.path(
    PASTA_SAIDA,
    "regras_completude_precipitacao_por_fonte.csv"
  ),
  bom = TRUE
)

# ------------------------------------------------------------
# 4. CARREGAR DADOS
# ------------------------------------------------------------

cat("\nCarregando base QA/QC da Etapa 9C...\n")

p <- readRDS(ARQ_QAQC)
setDT(p)

necessarias <- c(
  "fonte_dados",
  "cod_estacao",
  "datahora_local",
  "data_local",
  "ano",
  "precipitacao_mm",
  "conflito_timestamp",
  "valor_ausente",
  "valor_negativo",
  "valor_valido"
)

faltantes <- setdiff(necessarias, names(p))

if (length(faltantes) > 0) {
  stop(
    "A base QA/QC nao possui as colunas: ",
    paste(faltantes, collapse = ", ")
  )
}

p[, fonte_dados := as.character(fonte_dados)]
p[, cod_estacao := as.character(cod_estacao)]
p[, ano := as.integer(ano)]

if (!inherits(p$data_local, "IDate")) {
  p[, data_local := as.IDate(data_local)]
}

inv <- fread(
  ARQ_INVENTARIO,
  encoding = "UTF-8"
)

inv[, fonte_dados := as.character(fonte_dados)]
inv[, cod_estacao := as.character(cod_estacao)]

meta <- inv[
  ,
  .(
    nome_estacao = primeiro_texto(nome_estacao),
    municipios = primeiro_texto(municipios),
    tipo_estacao = primeiro_texto(tipo_estacao),
    periodicidade_fonte = primeiro_texto(
      periodicidade_fonte
    )
  ),
  by = .(
    fonte_dados,
    cod_estacao
  )
]

# Classificacao de tipo para uso na 9D.
meta[
  fonte_dados == "CEMADEN",
  tipo_regra := "CEMADEN"
]

meta[
  fonte_dados == "INMET" &
    grepl(
      "AUTOMAT",
      toupper(tipo_estacao)
    ),
  tipo_regra := "INMET_AUTOMATICA"
]

meta[
  fonte_dados == "INMET" &
    (
      grepl(
        "CONVENC",
        toupper(tipo_estacao)
      ) |
        is.na(tipo_regra)
    ),
  tipo_regra := "INMET_CONVENCIONAL"
]

# Mantem apenas registros necessarios para completude.
p <- p[
  ,
  .(
    fonte_dados,
    cod_estacao,
    datahora_local,
    data_local,
    ano,
    precipitacao_mm,
    conflito_timestamp,
    valor_ausente,
    valor_negativo,
    valor_valido
  )
]

# ------------------------------------------------------------
# 5. CEMADEN: COMPLETUDE POR LACUNAS TEMPORAIS
# ------------------------------------------------------------

cat("Calculando continuidade temporal CEMADEN...\n")

cem <- p[
  fonte_dados == "CEMADEN" &
    valor_valido == TRUE,
  .(
    fonte_dados,
    cod_estacao,
    ano,
    data_local,
    datahora_local
  )
]

# Garante um timestamp unico por estacao.
cem <- unique(
  cem,
  by = c(
    "fonte_dados",
    "cod_estacao",
    "datahora_local"
  )
)

setorder(
  cem,
  cod_estacao,
  datahora_local
)

# Intervalos apenas dentro do mesmo dia.
cem[
  ,
  delta_intra_min := as.numeric(
    difftime(
      datahora_local,
      shift(datahora_local),
      units = "mins"
    )
  ),
  by = .(
    cod_estacao,
    data_local
  )
]

cem_dia_obs <- cem[
  ,
  .(
    n_obs_validas = .N,
    primeira_obs_num = as.numeric(
      min(datahora_local)
    ),
    ultima_obs_num = as.numeric(
      max(datahora_local)
    ),
    maior_gap_intra_min = {
      z <- delta_intra_min[
        is.finite(delta_intra_min) &
          delta_intra_min > 0
      ]
      if (length(z) == 0) 0 else max(z)
    }
  ),
  by = .(
    fonte_dados,
    cod_estacao,
    ano,
    data_local
  )
]

# Cria grade completa de dias somente para estacao-anos
# que possuem pelo menos um valor valido.
cem_ea <- unique(
  cem_dia_obs[
    ,
    .(
      fonte_dados,
      cod_estacao,
      ano
    )
  ]
)

cem_cal <- cem_ea[
  ,
  .(
    data_local = seq(
      as.IDate(
        sprintf(
          "%04d-01-01",
          ano
        )
      ),
      as.IDate(
        sprintf(
          "%04d-12-31",
          ano
        )
      ),
      by = "day"
    )
  ),
  by = .(
    fonte_dados,
    cod_estacao,
    ano
  )
]

cem_dia <- merge(
  cem_cal,
  cem_dia_obs,
  by = c(
    "fonte_dados",
    "cod_estacao",
    "ano",
    "data_local"
  ),
  all.x = TRUE
)

cem_dia[
  is.na(n_obs_validas),
  n_obs_validas := 0L
]

# Ordena em toda a serie da estacao para recuperar
# a ultima observacao anterior e a primeira posterior,
# inclusive quando o dia intermediario esta sem registros.
setorder(
  cem_dia,
  cod_estacao,
  data_local
)

cem_dia[
  ,
  ultima_locf := nafill(
    ultima_obs_num,
    type = "locf"
  ),
  by = cod_estacao
]

cem_dia[
  ,
  primeira_nocb := nafill(
    primeira_obs_num,
    type = "nocb"
  ),
  by = cod_estacao
]

cem_dia[
  ,
  obs_anterior_num := shift(
    ultima_locf
  ),
  by = cod_estacao
]

cem_dia[
  ,
  obs_posterior_num := shift(
    primeira_nocb,
    type = "lead"
  ),
  by = cod_estacao
]

cem_dia[
  ,
  gap_inicio_min := fifelse(
    n_obs_validas > 0 &
      !is.na(obs_anterior_num) &
      !is.na(primeira_obs_num),
    (
      primeira_obs_num -
        obs_anterior_num
    ) / 60,
    NA_real_
  )
]

cem_dia[
  ,
  gap_fim_min := fifelse(
    n_obs_validas > 0 &
      !is.na(obs_posterior_num) &
      !is.na(ultima_obs_num),
    (
      obs_posterior_num -
        ultima_obs_num
    ) / 60,
    NA_real_
  )
]

cem_dia[
  ,
  maior_gap_dia_min := fifelse(
    n_obs_validas > 0 &
      !is.na(gap_inicio_min) &
      !is.na(gap_fim_min),
    pmax(
      maior_gap_intra_min,
      gap_inicio_min,
      gap_fim_min,
      na.rm = TRUE
    ),
    NA_real_
  )
]

cem_dia[
  ,
  `:=`(
    cobertura_primaria = fifelse(
      !is.na(maior_gap_dia_min),
      maior_gap_dia_min <= 60,
      FALSE
    ),
    cobertura_sens_1 = fifelse(
      !is.na(maior_gap_dia_min),
      maior_gap_dia_min <= 70,
      FALSE
    ),
    cobertura_sens_2 = fifelse(
      !is.na(maior_gap_dia_min),
      maior_gap_dia_min <= 90,
      FALSE
    ),
    criterio_primario = "CEMADEN_MAX_GAP_60MIN",
    criterio_sens_1 = "CEMADEN_MAX_GAP_70MIN",
    criterio_sens_2 = "CEMADEN_MAX_GAP_90MIN",
    elegivel_pareamento_primario = TRUE
  )
]

cem_dia[
  ,
  c(
    "ultima_locf",
    "primeira_nocb"
  ) := NULL
]

# Libera objetos CEMADEN de alta granularidade antes de trabalhar
# com o INMET, reduzindo o pico de memoria.
rm(
  cem,
  cem_dia_obs,
  cem_ea,
  cem_cal
)
gc()

# ------------------------------------------------------------
# 6. INMET AUTOMATICA: 24 OBSERVACOES HORARIAS
# ------------------------------------------------------------

cat("Calculando completude INMET automatica...\n")

# O INMET representa parcela muito menor da base. Subconjunta antes
# do merge com metadados para evitar duplicar toda a base de 20+ milhoes.
p_inmet <- p[
  fonte_dados == "INMET"
]

p_meta <- merge(
  p_inmet,
  meta[
    ,
    .(
      fonte_dados,
      cod_estacao,
      tipo_regra
    )
  ],
  by = c(
    "fonte_dados",
    "cod_estacao"
  ),
  all.x = TRUE
)

rm(
  p_inmet,
  p
)
gc()

ia <- p_meta[
  tipo_regra == "INMET_AUTOMATICA"
]

ia_obs <- ia[
  ,
  .(
    n_obs_validas = uniqueN(
      datahora_local[
        valor_valido == TRUE
      ]
    )
  ),
  by = .(
    fonte_dados,
    cod_estacao,
    ano,
    data_local
  )
]

ia_ea <- unique(
  ia[
    ,
    .(
      fonte_dados,
      cod_estacao,
      ano
    )
  ]
)

ia_cal <- ia_ea[
  ,
  .(
    data_local = seq(
      as.IDate(
        sprintf(
          "%04d-01-01",
          ano
        )
      ),
      as.IDate(
        sprintf(
          "%04d-12-31",
          ano
        )
      ),
      by = "day"
    )
  ),
  by = .(
    fonte_dados,
    cod_estacao,
    ano
  )
]

ia_dia <- merge(
  ia_cal,
  ia_obs,
  by = c(
    "fonte_dados",
    "cod_estacao",
    "ano",
    "data_local"
  ),
  all.x = TRUE
)

ia_dia[
  is.na(n_obs_validas),
  n_obs_validas := 0L
]

ia_dia[
  ,
  `:=`(
    maior_gap_dia_min = NA_real_,
    gap_inicio_min = NA_real_,
    gap_fim_min = NA_real_,
    maior_gap_intra_min = NA_real_,
    cobertura_primaria = n_obs_validas >= 24,
    cobertura_sens_1 = n_obs_validas >= 20,
    cobertura_sens_2 = n_obs_validas >= 18,
    criterio_primario = "INMET_AUTO_24H",
    criterio_sens_1 = "INMET_AUTO_20H",
    criterio_sens_2 = "INMET_AUTO_18H",
    elegivel_pareamento_primario = TRUE
  )
]

# ------------------------------------------------------------
# 7. INMET CONVENCIONAL: DISPONIBILIDADE DE ACUMULADO 24H
# ------------------------------------------------------------

cat("Inventariando INMET convencional...\n")

ic <- p_meta[
  tipo_regra == "INMET_CONVENCIONAL"
]

ic_obs <- ic[
  ,
  .(
    n_obs_validas = uniqueN(
      datahora_local[
        valor_valido == TRUE
      ]
    )
  ),
  by = .(
    fonte_dados,
    cod_estacao,
    ano,
    data_local
  )
]

ic_ea <- unique(
  ic[
    ,
    .(
      fonte_dados,
      cod_estacao,
      ano
    )
  ]
)

ic_cal <- ic_ea[
  ,
  .(
    data_local = seq(
      as.IDate(
        sprintf(
          "%04d-01-01",
          ano
        )
      ),
      as.IDate(
        sprintf(
          "%04d-12-31",
          ano
        )
      ),
      by = "day"
    )
  ),
  by = .(
    fonte_dados,
    cod_estacao,
    ano
  )
]

ic_dia <- merge(
  ic_cal,
  ic_obs,
  by = c(
    "fonte_dados",
    "cod_estacao",
    "ano",
    "data_local"
  ),
  all.x = TRUE
)

ic_dia[
  is.na(n_obs_validas),
  n_obs_validas := 0L
]

ic_dia[
  ,
  `:=`(
    maior_gap_dia_min = NA_real_,
    gap_inicio_min = NA_real_,
    gap_fim_min = NA_real_,
    maior_gap_intra_min = NA_real_,
    cobertura_primaria = NA,
    cobertura_sens_1 = NA,
    cobertura_sens_2 = NA,
    criterio_primario =
      "INMET_CONV_24H_PENDENTE_ALINHAMENTO",
    criterio_sens_1 = NA_character_,
    criterio_sens_2 = NA_character_,
    elegivel_pareamento_primario = FALSE,
    disponibilidade_24h_convencional =
      n_obs_validas > 0
  )
]

# Campos comuns.
cem_dia[
  ,
  disponibilidade_24h_convencional := NA
]

ia_dia[
  ,
  disponibilidade_24h_convencional := NA
]

# ------------------------------------------------------------
# 8. BASE DIARIA PADRONIZADA
# ------------------------------------------------------------

col_comuns <- c(
  "fonte_dados",
  "cod_estacao",
  "ano",
  "data_local",
  "n_obs_validas",
  "maior_gap_dia_min",
  "gap_inicio_min",
  "gap_fim_min",
  "maior_gap_intra_min",
  "cobertura_primaria",
  "cobertura_sens_1",
  "cobertura_sens_2",
  "criterio_primario",
  "criterio_sens_1",
  "criterio_sens_2",
  "elegivel_pareamento_primario",
  "disponibilidade_24h_convencional"
)

diario <- rbindlist(
  list(
    cem_dia[
      ,
      ..col_comuns
    ],
    ia_dia[
      ,
      ..col_comuns
    ],
    ic_dia[
      ,
      ..col_comuns
    ]
  ),
  use.names = TRUE,
  fill = TRUE
)

diario <- merge(
  diario,
  meta,
  by = c(
    "fonte_dados",
    "cod_estacao"
  ),
  all.x = TRUE
)

setorder(
  diario,
  fonte_dados,
  cod_estacao,
  data_local
)

saveRDS(
  diario,
  ARQ_DIARIO_RDS,
  compress = "gzip"
)

# ------------------------------------------------------------
# 9. RESUMO ANUAL POR FONTE
# ------------------------------------------------------------

cat("Gerando resumo anual por fonte...\n")

anual <- diario[
  ,
  .(
    n_dias_calendario = .N,
    n_dias_com_valor = sum(
      n_obs_validas > 0
    ),
    n_dias_cobertura_primaria = sum(
      cobertura_primaria %in% TRUE
    ),
    n_dias_sens_1 = sum(
      cobertura_sens_1 %in% TRUE
    ),
    n_dias_sens_2 = sum(
      cobertura_sens_2 %in% TRUE
    ),
    prop_cobertura_primaria = prop_true(
      cobertura_primaria
    ),
    prop_sens_1 = prop_true(
      cobertura_sens_1
    ),
    prop_sens_2 = prop_true(
      cobertura_sens_2
    ),
    n_dias_disponibilidade_convencional = sum(
      disponibilidade_24h_convencional %in% TRUE
    ),
    maior_lacuna_dias_sem_cobertura_primaria =
      if (
        all(
          is.na(
            cobertura_primaria
          )
        )
      ) {
        NA_integer_
      } else {
        maior_lacuna_false(
          cobertura_primaria
        )
      },
    p95_maior_gap_dia_min = {
      z <- maior_gap_dia_min[
        is.finite(
          maior_gap_dia_min
        )
      ]
      if (
        length(z) == 0
      ) {
        NA_real_
      } else {
        as.numeric(
          quantile(
            z,
            0.95,
            na.rm = TRUE,
            names = FALSE
          )
        )
      }
    }
  ),
  by = .(
    fonte_dados,
    cod_estacao,
    nome_estacao,
    municipios,
    tipo_regra,
    tipo_estacao,
    ano,
    criterio_primario,
    elegivel_pareamento_primario
  )
]

setorder(
  anual,
  fonte_dados,
  cod_estacao,
  ano
)

fwrite(
  anual,
  file.path(
    PASTA_SAIDA,
    "completude_anual_precipitacao_por_fonte.csv"
  ),
  bom = TRUE
)

# ------------------------------------------------------------
# 10. SOBREPOSICAO COM DIAS VALIDOS DE MP10
# ------------------------------------------------------------

cat("Calculando sobreposicao com MP10...\n")

cand <- fread(
  ARQ_CANDIDATOS,
  encoding = "UTF-8"
)

mp10 <- fread(
  ARQ_MP10,
  encoding = "UTF-8"
)

mp10[
  ,
  `:=`(
    ano = as.integer(
      ano
    ),
    data_local = as.IDate(
      data
    )
  )
]

mp10_validos <- mp10[
  dia_valido_mma_16h == TRUE,
  .(
    estacao_historica = as.character(
      no_estacao
    ),
    ano_alvo = ano,
    data_local
  )
]

# Uma linha por fonte + codigo + alvo.
cand_id <- cand[
  distancia_km <= 50,
  .(
    estacao_mp10 = primeiro_texto(
      estacao_mp10
    ),
    municipio_mp10 = primeiro_texto(
      municipio_mp10
    ),
    distancia_km = min(
      distancia_km,
      na.rm = TRUE
    ),
    nota = primeiro_texto(
      nota
    )
  ),
  by = .(
    estacao_historica,
    ano_alvo,
    fonte_dados,
    cod_estacao
  )
]

pares <- merge(
  cand_id,
  mp10_validos,
  by = c(
    "estacao_historica",
    "ano_alvo"
  ),
  allow.cartesian = TRUE
)

dj <- diario[
  ,
  .(
    fonte_dados,
    cod_estacao,
    ano_alvo = ano,
    data_local,
    n_obs_validas,
    cobertura_primaria,
    cobertura_sens_1,
    cobertura_sens_2,
    elegivel_pareamento_primario,
    disponibilidade_24h_convencional,
    tipo_regra,
    criterio_primario
  )
]

pares <- merge(
  pares,
  dj,
  by = c(
    "fonte_dados",
    "cod_estacao",
    "ano_alvo",
    "data_local"
  ),
  all.x = TRUE
)

sobreposicao <- pares[
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
    estacao_historica,
    ano_alvo,
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
  cand_id,
  sobreposicao,
  by = c(
    "estacao_historica",
    "ano_alvo",
    "fonte_dados",
    "cod_estacao"
  ),
  all.x = TRUE
)

avaliacao <- merge(
  avaliacao,
  meta,
  by = c(
    "fonte_dados",
    "cod_estacao"
  ),
  all.x = TRUE
)

avaliacao[
  ,
  faixa_operacional := cut(
    distancia_km,
    breaks = c(
      -Inf,
      5,
      10,
      20,
      30,
      50,
      Inf
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
  avaliacao,
  estacao_historica,
  ano_alvo,
  distancia_km,
  -pct_overlap_primario
)

fwrite(
  avaliacao,
  file.path(
    PASTA_SAIDA,
    "sobreposicao_mp10_precipitacao_por_fonte.csv"
  ),
  bom = TRUE
)

for (limite in c(10, 20, 30)) {

  tab <- avaliacao[
    distancia_km <= limite
  ]

  fwrite(
    tab,
    file.path(
      PASTA_SAIDA,
      sprintf(
        "candidatos_precipitacao_ate_%dkm_por_fonte.csv",
        limite
      )
    ),
    bom = TRUE
  )
}

# ------------------------------------------------------------
# 11. LISTA CURTA SEM RANKING GLOBAL
# ------------------------------------------------------------

# Mantem apenas candidatos primarios ate 20 km.
# A tabela e ordenada primeiro por alvo, depois por distancia;
# a cobertura e apresentada como criterio paralelo.
lista_curta <- avaliacao[
  distancia_km <= 20 &
    elegivel_pareamento_primario == TRUE &
    !is.na(
      pct_overlap_primario
    )
][
  order(
    estacao_historica,
    ano_alvo,
    distancia_km,
    -pct_overlap_primario
  )
]

fwrite(
  lista_curta,
  file.path(
    PASTA_SAIDA,
    "lista_curta_candidatos_ate20km.csv"
  ),
  bom = TRUE
)

# ------------------------------------------------------------
# 12. RESUMO
# ------------------------------------------------------------

resumo <- data.table(
  indicador = c(
    "n_estacoes_cemaden",
    "n_estacoes_inmet_automaticas",
    "n_estacoes_inmet_convencionais",
    "n_estacao_ano_cemaden",
    "n_estacao_ano_inmet_auto",
    "n_estacao_ano_inmet_conv",
    "n_candidatos_alvos_ate50km",
    "n_candidatos_primarios_ate20km",
    "n_alvos_mp10_avaliados"
  ),
  valor = c(
    uniqueN(
      meta[
        tipo_regra == "CEMADEN",
        cod_estacao
      ]
    ),
    uniqueN(
      meta[
        tipo_regra == "INMET_AUTOMATICA",
        cod_estacao
      ]
    ),
    uniqueN(
      meta[
        tipo_regra == "INMET_CONVENCIONAL",
        cod_estacao
      ]
    ),
    uniqueN(
      anual[
        tipo_regra == "CEMADEN",
        paste(
          cod_estacao,
          ano,
          sep = "::"
        )
      ]
    ),
    uniqueN(
      anual[
        tipo_regra == "INMET_AUTOMATICA",
        paste(
          cod_estacao,
          ano,
          sep = "::"
        )
      ]
    ),
    uniqueN(
      anual[
        tipo_regra == "INMET_CONVENCIONAL",
        paste(
          cod_estacao,
          ano,
          sep = "::"
        )
      ]
    ),
    nrow(
      avaliacao[
        distancia_km <= 50
      ]
    ),
    nrow(
      lista_curta
    ),
    uniqueN(
      avaliacao[
        ,
        paste(
          estacao_historica,
          ano_alvo,
          sep = "::"
        )
      ]
    )
  )
)

fwrite(
  resumo,
  file.path(
    PASTA_SAIDA,
    "resumo_execucao_completude_por_fonte.csv"
  ),
  bom = TRUE
)

cat("\n============================================================\n")
cat("ETAPA 9D - COMPLETUDE POR FONTE CONCLUIDA\n")
cat("============================================================\n\n")

print(resumo)

cat("\nLista curta ate 20 km:\n")

if (nrow(lista_curta) > 0) {
  print(
    lista_curta[
      ,
      .(
        estacao_historica,
        ano_alvo,
        fonte_dados,
        cod_estacao,
        nome_estacao,
        tipo_regra,
        distancia_km,
        n_dias_mp10_validos,
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
cat(PASTA_SAIDA, "\n")

cat("\nBase diaria local:\n")
cat(ARQ_DIARIO_RDS, "\n")

cat(
  "\nIMPORTANTE:\n",
  "- CEMADEN foi avaliado por continuidade temporal, nao por 24 registros/dia;\n",
  "- INMET automatico foi avaliado como serie horaria;\n",
  "- INMET convencional permanece secundario ate confirmar o horario do acumulado 24 h;\n",
  "- nenhuma estacao foi escolhida automaticamente como pareamento final.\n",
  sep = ""
)
