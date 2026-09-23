# ============================================================
# ETAPA 9H - SERIES DIARIAS E COERENCIA DOS CANDIDATOS DE GAIBU 2019
# Projeto de Mestrado - MP10 / Suape
# ============================================================
#
# OBJETIVO
# Construir as series diarias de precipitacao dos candidatos
# identificados na Etapa 9G para Gaibu 2019, usando a convencao
# temporal final da Etapa 9F, e comparar a coerencia entre as
# estacoes antes de definir o pareamento pluviometrico final.
#
# CONVENCAO TEMPORAL
# - usa apenas registros CEMADEN previamente validos na Etapa 9C;
# - registros exatamente a 00:00 local sao atribuidos ao dia civil
#   anterior;
# - usa dados de 01/01/2020 00:00 quando disponiveis para completar
#   31/12/2019;
# - mantem a completude da Etapa 9D:
#   <=60 min principal, <=70 e <=90 min sensibilidades.
#
# SAIDAS PRINCIPAIS
# - series diarias dos 12 candidatos ate 20 km;
# - resumo por candidato;
# - comparacao pareada de todos os candidatos;
# - comparacao de um nucleo prioritario:
#   Enseado dos Corais, Barra de Jangada, Ruropolis e Prazeres;
# - comparacao de cada candidato com Enseado dos Corais, a estacao
#   praticamente colocalizada com Gaibu.
#
# NAO FAZ
# - imputacao de chuva;
# - media entre pluviometros;
# - exclusao automatica de extremos;
# - escolha automatica do pareamento final.
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
  "qaqc_completude", "precipitacao_2017_2025_qaqc_v2.rds"
)

ARQ_COMP_DIA <- file.path(
  "outputs", "09_meteorologia", "precipitacao",
  "completude_por_fonte",
  "completude_diaria_precipitacao_por_fonte.rds"
)

ARQ_CAND <- file.path(
  "outputs", "09_meteorologia", "precipitacao",
  "gaibu_2019", "lista_curta_gaibu_2019_ate20km.csv"
)

ARQ_MP10 <- file.path(
  "outputs", "04_completude",
  "completude_diaria_mp10.csv"
)

SAIDA <- file.path(
  "outputs", "09_meteorologia", "precipitacao",
  "gaibu_2019", "series_diarias"
)

dir.create(SAIDA, recursive = TRUE, showWarnings = FALSE)

for (f in c(ARQ_QAQC, ARQ_COMP_DIA, ARQ_CAND, ARQ_MP10)) {
  if (!file.exists(f)) {
    stop("Arquivo necessario nao encontrado: ", f)
  }
}

# ------------------------------------------------------------
# 2. CONSTANTES E FUNCOES
# ------------------------------------------------------------

ANO_ALVO <- 2019L
ESTACAO_MP10 <- "GAIBU"
COD_REFERENCIA_LOCAL <- "260290204A" # Enseado dos Corais

COD_NUCLEO <- c(
  "260290204A", # Enseado dos Corais
  "260790109A", # Barra de Jangada
  "260720804A", # Ruropolis
  "260790104A"  # Prazeres
)

cor_segura <- function(x, y, metodo = "pearson") {
  ok <- is.finite(x) & is.finite(y)
  if (sum(ok) < 10) return(NA_real_)
  if (sd(x[ok]) == 0 || sd(y[ok]) == 0) return(NA_real_)
  suppressWarnings(cor(x[ok], y[ok], method = metodo))
}

mae <- function(x) {
  x <- x[is.finite(x)]
  if (!length(x)) return(NA_real_)
  mean(abs(x))
}

rmse <- function(x) {
  x <- x[is.finite(x)]
  if (!length(x)) return(NA_real_)
  sqrt(mean(x^2))
}

jaccard_binario <- function(a, b) {
  a <- a %in% TRUE
  b <- b %in% TRUE
  u <- sum(a | b)
  if (u == 0) return(NA_real_)
  sum(a & b) / u
}

metricas_par <- function(d, flag_a, flag_b, criterio) {

  ok <- (
    d[[flag_a]] %in% TRUE &
      d[[flag_b]] %in% TRUE &
      is.finite(d$precip_a) &
      is.finite(d$precip_b)
  )

  ok_mp10 <- ok & (d$dia_mp10_valido %in% TRUE)

  x <- d$precip_a
  y <- d$precip_b

  dif <- y[ok] - x[ok]
  dif_mp10 <- y[ok_mp10] - x[ok_mp10]

  wet0_a <- x[ok] > 0
  wet0_b <- y[ok] > 0
  wet1_a <- x[ok] >= 1
  wet1_b <- y[ok] >= 1

  data.table(
    criterio = criterio,

    n_dias_comuns = sum(ok),
    n_dias_comuns_mp10 = sum(ok_mp10),

    pct_dias_mp10_comuns = if (
      sum(d$dia_mp10_valido %in% TRUE) > 0
    ) {
      100 * sum(ok_mp10) /
        sum(d$dia_mp10_valido %in% TRUE)
    } else {
      NA_real_
    },

    correlacao_pearson = cor_segura(
      x[ok], y[ok], "pearson"
    ),

    correlacao_spearman = cor_segura(
      x[ok], y[ok], "spearman"
    ),

    correlacao_pearson_mp10 = cor_segura(
      x[ok_mp10], y[ok_mp10], "pearson"
    ),

    correlacao_spearman_mp10 = cor_segura(
      x[ok_mp10], y[ok_mp10], "spearman"
    ),

    total_a_comum_mm = if (
      sum(ok)
    ) {
      sum(x[ok])
    } else {
      NA_real_
    },

    total_b_comum_mm = if (
      sum(ok)
    ) {
      sum(y[ok])
    } else {
      NA_real_
    },

    razao_total_b_a = if (
      sum(ok) &&
        sum(x[ok]) > 0
    ) {
      sum(y[ok]) / sum(x[ok])
    } else {
      NA_real_
    },

    vies_b_menos_a_mm = if (
      length(dif)
    ) {
      mean(dif)
    } else {
      NA_real_
    },

    mae_mm = mae(dif),
    rmse_mm = rmse(dif),

    vies_b_menos_a_mp10_mm = if (
      length(dif_mp10)
    ) {
      mean(dif_mp10)
    } else {
      NA_real_
    },

    mae_mp10_mm = mae(dif_mp10),
    rmse_mp10_mm = rmse(dif_mp10),

    acordo_chuva_maior_0 = if (
      length(wet0_a)
    ) {
      mean(wet0_a == wet0_b)
    } else {
      NA_real_
    },

    jaccard_chuva_maior_0 = jaccard_binario(
      wet0_a, wet0_b
    ),

    acordo_chuva_maior_igual_1mm = if (
      length(wet1_a)
    ) {
      mean(wet1_a == wet1_b)
    } else {
      NA_real_
    },

    jaccard_chuva_maior_igual_1mm = jaccard_binario(
      wet1_a, wet1_b
    )
  )
}

# ------------------------------------------------------------
# 3. CANDIDATOS
# ------------------------------------------------------------

cat("\nCarregando candidatos de Gaibu 2019...\n")

cand <- fread(
  ARQ_CAND,
  encoding = "UTF-8"
)

cand[
  ,
  `:=`(
    cod_estacao = as.character(
      cod_estacao
    ),
    ano_alvo = as.integer(
      ano_alvo
    )
  )
]

cand <- unique(
  cand[
    fonte_dados == "CEMADEN" &
      ano_alvo == ANO_ALVO &
      elegivel_pareamento_primario_mp10 == TRUE,
    .(
      cod_estacao,
      nome_estacao,
      municipios,
      latitude,
      longitude,
      distancia_gaibu_km,
      pct_dias_precip_com_valor,
      pct_overlap_primario,
      pct_overlap_sens_1,
      pct_overlap_sens_2
    )
  ]
)

if (!nrow(cand)) {
  stop("Nenhum candidato CEMADEN elegivel de Gaibu 2019.")
}

# ------------------------------------------------------------
# 4. PRECIPITACAO DIARIA COM CONVENCAO FINAL
# ------------------------------------------------------------

cat("Construindo precipitacao diaria com a convencao final...\n")

p <- readRDS(ARQ_QAQC)
setDT(p)

p[
  ,
  `:=`(
    cod_estacao = as.character(
      cod_estacao
    ),
    ano = as.integer(
      ano
    )
  )
]

p <- p[
  fonte_dados == "CEMADEN" &
    valor_valido == TRUE &
    cod_estacao %in% cand$cod_estacao &
    ano %in% c(
      ANO_ALVO,
      ANO_ALVO + 1L
    )
]

if (!nrow(p)) {
  stop("Nenhum registro valido encontrado para os candidatos.")
}

p[
  ,
  hora_local_txt := format(
    datahora_local,
    "%H:%M:%S",
    tz = "America/Recife"
  )
]

p[
  ,
  data_referencia := as.IDate(
    data_local
  )
]

p[
  hora_local_txt == "00:00:00",
  data_referencia := data_referencia - 1L
]

p[
  ,
  ano_referencia := as.integer(
    format(
      data_referencia,
      "%Y"
    )
  )
]

chuva_dia <- p[
  ano_referencia == ANO_ALVO,
  .(
    precip_diaria_mm = sum(
      precipitacao_mm,
      na.rm = TRUE
    ),
    n_registros_validos = .N,
    n_registros_00h_relocados = sum(
      hora_local_txt == "00:00:00"
    ),
    n_registros_chuva = sum(
      precipitacao_mm > 0,
      na.rm = TRUE
    ),
    max_acumulado_intervalo_mm = max(
      precipitacao_mm,
      na.rm = TRUE
    )
  ),
  by = .(
    cod_estacao,
    data_local = data_referencia
  )
]

rm(p)
gc()

# ------------------------------------------------------------
# 5. COMPLETUDE DA 9D
# ------------------------------------------------------------

cat("Integrando completude temporal da Etapa 9D...\n")

comp <- readRDS(ARQ_COMP_DIA)
setDT(comp)

comp[
  ,
  `:=`(
    cod_estacao = as.character(
      cod_estacao
    ),
    ano = as.integer(
      ano
    )
  )
]

comp <- comp[
  fonte_dados == "CEMADEN" &
    ano == ANO_ALVO &
    cod_estacao %in% cand$cod_estacao,
  .(
    cod_estacao,
    data_local,
    n_obs_validas,
    maior_gap_dia_min,
    cobertura_60 = cobertura_primaria,
    cobertura_70 = cobertura_sens_1,
    cobertura_90 = cobertura_sens_2
  )
]

# ------------------------------------------------------------
# 6. MP10 GAIBU 2019
# ------------------------------------------------------------

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
    ),
    estacao_mp10 = toupper(
      as.character(
        no_estacao
      )
    )
  )
]

mp10 <- mp10[
  ano == ANO_ALVO &
    estacao_mp10 == ESTACAO_MP10,
  .(
    data_local,
    dia_mp10_valido = dia_valido_mma_16h,
    media_mp10 = media_24h_mma
  )
]

if (!nrow(mp10)) {
  stop("Gaibu 2019 nao foi localizada na base diaria de MP10.")
}

# ------------------------------------------------------------
# 7. GRADE DIARIA DOS CANDIDATOS
# ------------------------------------------------------------

cat("Construindo series diarias de Gaibu 2019...\n")

grade <- cand[
  ,
  .(
    data_local = seq(
      as.IDate("2019-01-01"),
      as.IDate("2019-12-31"),
      by = "day"
    )
  ),
  by = .(
    cod_estacao,
    nome_estacao,
    municipios,
    latitude,
    longitude,
    distancia_gaibu_km,
    pct_dias_precip_com_valor,
    pct_overlap_primario,
    pct_overlap_sens_1,
    pct_overlap_sens_2
  )
]

serie <- merge(
  grade,
  chuva_dia,
  by = c(
    "cod_estacao",
    "data_local"
  ),
  all.x = TRUE
)

serie <- merge(
  serie,
  comp,
  by = c(
    "cod_estacao",
    "data_local"
  ),
  all.x = TRUE
)

serie <- merge(
  serie,
  mp10,
  by = "data_local",
  all.x = TRUE
)

serie[
  ,
  tem_precipitacao := !is.na(
    precip_diaria_mm
  )
]

setorder(
  serie,
  distancia_gaibu_km,
  cod_estacao,
  data_local
)

fwrite(
  serie,
  file.path(
    SAIDA,
    "series_diarias_gaibu_2019.csv"
  ),
  bom = TRUE
)

# ------------------------------------------------------------
# 8. RESUMO INDIVIDUAL
# ------------------------------------------------------------

resumo_cand <- serie[
  ,
  .(
    distancia_gaibu_km = unique(
      distancia_gaibu_km
    )[1],

    n_dias_ano = .N,

    n_dias_com_precipitacao = sum(
      tem_precipitacao
    ),

    n_dias_cobertura_60 = sum(
      cobertura_60 %in% TRUE
    ),

    n_dias_cobertura_70 = sum(
      cobertura_70 %in% TRUE
    ),

    n_dias_cobertura_90 = sum(
      cobertura_90 %in% TRUE
    ),

    n_dias_mp10_validos = sum(
      dia_mp10_valido %in% TRUE
    ),

    n_overlap_60_mp10 = sum(
      cobertura_60 %in% TRUE &
        dia_mp10_valido %in% TRUE
    ),

    n_overlap_70_mp10 = sum(
      cobertura_70 %in% TRUE &
        dia_mp10_valido %in% TRUE
    ),

    n_overlap_90_mp10 = sum(
      cobertura_90 %in% TRUE &
        dia_mp10_valido %in% TRUE
    ),

    total_mm_dias_60 = sum(
      precip_diaria_mm[
        cobertura_60 %in% TRUE
      ],
      na.rm = TRUE
    ),

    total_mm_dias_70 = sum(
      precip_diaria_mm[
        cobertura_70 %in% TRUE
      ],
      na.rm = TRUE
    ),

    total_mm_dias_90 = sum(
      precip_diaria_mm[
        cobertura_90 %in% TRUE
      ],
      na.rm = TRUE
    ),

    max_precip_diaria_mm = if (
      all(
        is.na(
          precip_diaria_mm
        )
      )
    ) {
      NA_real_
    } else {
      max(
        precip_diaria_mm,
        na.rm = TRUE
      )
    },

    n_registros_00h_relocados = sum(
      n_registros_00h_relocados,
      na.rm = TRUE
    )
  ),
  by = .(
    cod_estacao,
    nome_estacao,
    municipios
  )
]

resumo_cand[
  ,
  `:=`(
    pct_overlap_60_mp10 =
      100 *
        n_overlap_60_mp10 /
        n_dias_mp10_validos,

    pct_overlap_70_mp10 =
      100 *
        n_overlap_70_mp10 /
        n_dias_mp10_validos,

    pct_overlap_90_mp10 =
      100 *
        n_overlap_90_mp10 /
        n_dias_mp10_validos
  )
]

setorder(
  resumo_cand,
  distancia_gaibu_km
)

fwrite(
  resumo_cand,
  file.path(
    SAIDA,
    "resumo_candidatos_gaibu_2019.csv"
  ),
  bom = TRUE
)

# ------------------------------------------------------------
# 9. COMPARACOES PAREADAS
# ------------------------------------------------------------

cat("Comparando pares de pluviometros...\n")

cc <- cand[
  order(
    distancia_gaibu_km,
    cod_estacao
  )
]

resultados <- list()
idx <- 0L

if (nrow(cc) >= 2) {

  cmb <- combn(
    seq_len(
      nrow(cc)
    ),
    2
  )

  for (jj in seq_len(ncol(cmb))) {

    ia <- cmb[1, jj]
    ib <- cmb[2, jj]

    ca <- cc[ia]
    cb <- cc[ib]

    a <- serie[
      cod_estacao == ca$cod_estacao,
      .(
        data_local,
        precip_a = precip_diaria_mm,
        cobertura_60_a = cobertura_60,
        cobertura_70_a = cobertura_70,
        cobertura_90_a = cobertura_90,
        dia_mp10_valido
      )
    ]

    b <- serie[
      cod_estacao == cb$cod_estacao,
      .(
        data_local,
        precip_b = precip_diaria_mm,
        cobertura_60_b = cobertura_60,
        cobertura_70_b = cobertura_70,
        cobertura_90_b = cobertura_90
      )
    ]

    d <- merge(
      a,
      b,
      by = "data_local",
      all = TRUE
    )

    m <- rbindlist(
      list(
        metricas_par(
          d,
          "cobertura_60_a",
          "cobertura_60_b",
          "max_gap_60min"
        ),
        metricas_par(
          d,
          "cobertura_70_a",
          "cobertura_70_b",
          "max_gap_70min"
        ),
        metricas_par(
          d,
          "cobertura_90_a",
          "cobertura_90_b",
          "max_gap_90min"
        )
      )
    )

    m[
      ,
      `:=`(
        cod_estacao_a = ca$cod_estacao,
        nome_estacao_a = ca$nome_estacao,
        distancia_a_km = ca$distancia_gaibu_km,
        cod_estacao_b = cb$cod_estacao,
        nome_estacao_b = cb$nome_estacao,
        distancia_b_km = cb$distancia_gaibu_km
      )
    ]

    idx <- idx + 1L
    resultados[[idx]] <- m
  }
}

pares <- if (
  length(resultados)
) {
  rbindlist(
    resultados,
    fill = TRUE
  )
} else {
  data.table()
}

if (nrow(pares)) {
  setorder(
    pares,
    criterio,
    distancia_a_km,
    distancia_b_km
  )
}

fwrite(
  pares,
  file.path(
    SAIDA,
    "comparacao_pareada_gaibu_2019.csv"
  ),
  bom = TRUE
)

# ------------------------------------------------------------
# 10. NUCLEO PRIORITARIO
# ------------------------------------------------------------

nucleo <- pares[
  cod_estacao_a %in% COD_NUCLEO &
    cod_estacao_b %in% COD_NUCLEO
]

fwrite(
  nucleo,
  file.path(
    SAIDA,
    "comparacao_nucleo_prioritario_gaibu_2019.csv"
  ),
  bom = TRUE
)

# ------------------------------------------------------------
# 11. COMPARACAO COM ENSEADO DOS CORAIS
# ------------------------------------------------------------

ref_local <- rbindlist(
  list(
    pares[
      cod_estacao_a == COD_REFERENCIA_LOCAL
    ],
    pares[
      cod_estacao_b == COD_REFERENCIA_LOCAL
    ]
  ),
  fill = TRUE
)

fwrite(
  ref_local,
  file.path(
    SAIDA,
    "comparacao_com_enseado_dos_corais.csv"
  ),
  bom = TRUE
)

# ------------------------------------------------------------
# 12. TABELA DE APOIO A DECISAO
# ------------------------------------------------------------

# Resume, para cada candidato, a coerencia com Enseado dos Corais
# no criterio principal e nas sensibilidades. Nao cria escore.
apoio <- copy(
  resumo_cand
)

coerencia_ref <- list()

for (crit in c(
  "max_gap_60min",
  "max_gap_70min",
  "max_gap_90min"
)) {

  tmp <- ref_local[
    criterio == crit
  ]

  tmp[
    ,
    candidato := fifelse(
      cod_estacao_a == COD_REFERENCIA_LOCAL,
      cod_estacao_b,
      cod_estacao_a
    )
  ]

  tmp[
    ,
    correlacao_ref := correlacao_pearson_mp10
  ]

  tmp[
    ,
    n_comuns_ref := n_dias_comuns_mp10
  ]

  coerencia_ref[[crit]] <- tmp[
    ,
    .(
      cod_estacao = candidato,
      correlacao_ref,
      n_comuns_ref
    )
  ]
}

if (length(coerencia_ref)) {

  c60 <- coerencia_ref[["max_gap_60min"]]
  c70 <- coerencia_ref[["max_gap_70min"]]
  c90 <- coerencia_ref[["max_gap_90min"]]

  if (!is.null(c60)) {
    setnames(
      c60,
      c("correlacao_ref", "n_comuns_ref"),
      c("corr_ref_60", "n_comuns_ref_60")
    )
    apoio <- merge(
      apoio,
      c60,
      by = "cod_estacao",
      all.x = TRUE
    )
  }

  if (!is.null(c70)) {
    setnames(
      c70,
      c("correlacao_ref", "n_comuns_ref"),
      c("corr_ref_70", "n_comuns_ref_70")
    )
    apoio <- merge(
      apoio,
      c70,
      by = "cod_estacao",
      all.x = TRUE
    )
  }

  if (!is.null(c90)) {
    setnames(
      c90,
      c("correlacao_ref", "n_comuns_ref"),
      c("corr_ref_90", "n_comuns_ref_90")
    )
    apoio <- merge(
      apoio,
      c90,
      by = "cod_estacao",
      all.x = TRUE
    )
  }
}

# A estacao de referencia local recebe identificacao explicita.
apoio[
  cod_estacao == COD_REFERENCIA_LOCAL,
  `:=`(
    corr_ref_60 = 1,
    corr_ref_70 = 1,
    corr_ref_90 = 1
  )
]

setorder(
  apoio,
  distancia_gaibu_km
)

fwrite(
  apoio,
  file.path(
    SAIDA,
    "apoio_decisao_pareamento_gaibu_2019.csv"
  ),
  bom = TRUE
)

# ------------------------------------------------------------
# 13. RESUMO
# ------------------------------------------------------------

resumo_exec <- data.table(
  indicador = c(
    "n_candidatos_gaibu",
    "n_linhas_series_diarias",
    "n_comparacoes_pareadas_criterio",
    "n_comparacoes_nucleo_prioritario",
    "n_comparacoes_com_enseado",
    "n_dias_mp10_validos_gaibu"
  ),
  valor = c(
    nrow(cand),
    nrow(serie),
    nrow(pares),
    nrow(nucleo),
    nrow(ref_local),
    sum(
      mp10$dia_mp10_valido %in% TRUE
    )
  )
)

fwrite(
  resumo_exec,
  file.path(
    SAIDA,
    "resumo_execucao_series_gaibu_2019.csv"
  ),
  bom = TRUE
)

cat("\n============================================================\n")
cat("ETAPA 9H - SERIES E COERENCIA DE GAIBU 2019 CONCLUIDA\n")
cat("============================================================\n\n")

print(resumo_exec)

cat("\nResumo dos candidatos:\n")
print(
  resumo_cand[
    ,
    .(
      cod_estacao,
      nome_estacao,
      distancia_gaibu_km,
      pct_overlap_60_mp10,
      pct_overlap_70_mp10,
      pct_overlap_90_mp10,
      max_precip_diaria_mm
    )
  ]
)

cat(
  "\nIMPORTANTE:\n",
  "- Enseado dos Corais e a referencia espacial local, nao uma escolha automatica;\n",
  "- comparar proximidade, cobertura e coerencia antes do pareamento final;\n",
  "- SUAPE 2022 continua pendente de coordenada documental valida.\n",
  sep = ""
)
