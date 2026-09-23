# ============================================================
# ETAPA 9E - SERIES DIARIAS E COERENCIA ENTRE PLUVIOMETROS CEMADEN
# Projeto de Mestrado - MP10 / Suape
# ============================================================
#
# OBJETIVO
# Construir as series diarias de precipitacao para os candidatos
# CEMADEN selecionados na Etapa 9D e comparar estacoes proximas
# antes de definir o pareamento meteorologico final.
#
# FUNDAMENTO OPERACIONAL
# Os dados CEMADEN representam acumulados de chuva transmitidos em
# resolucao variavel: tipicamente a cada 10 min durante chuva e
# uma vez por hora em ausencia de chuva. Por isso:
# - o total diario e a soma dos acumulados validos do dia;
# - a completude NAO depende de 24 registros/dia;
# - sao preservados os indicadores de continuidade da Etapa 9D:
#   max gap <=60 min (principal), <=70 e <=90 min (sensibilidades).
#
# ESTA ETAPA:
# 1. agrega os valores validos CEMADEN por dia;
# 2. preserva as flags de continuidade 60/70/90 min;
# 3. cruza as series com os dias validos de MP10;
# 4. compara todos os pares de candidatos ate 20 km;
# 5. calcula correlacao, vies, MAE, RMSE e concordancia de dias chuvosos;
# 6. produz uma tabela especifica do nucleo local de Ipojuca
#    (Centro, Ruropolis e Campo do Aviao), quando disponivel.
#
# NAO FAZ:
# - exclusao automatica de extremos;
# - imputacao de chuva;
# - media entre pluviometros;
# - escolha automatica da estacao final.
#
# Convencao temporal:
# Os valores sao atribuidos a data local do timestamp ja criada na
# Etapa 9A. Uma analise de sensibilidade separada avalia o efeito
# de atribuir registros exatamente a 00:00 ao dia anterior, pois
# o timestamp representa o fim do intervalo de acumulacao.
# ============================================================

if (!requireNamespace("data.table", quietly = TRUE)) {
  stop("Instale o pacote 'data.table' antes de executar.")
}
library(data.table)

ARQ_QAQC <- file.path(
  "outputs", "09_meteorologia", "precipitacao",
  "qaqc_completude", "precipitacao_2017_2025_qaqc_v2.rds"
)

ARQ_COMP_DIA <- file.path(
  "outputs", "09_meteorologia", "precipitacao",
  "completude_por_fonte",
  "completude_diaria_precipitacao_por_fonte.rds"
)

ARQ_LISTA <- file.path(
  "outputs", "09_meteorologia", "precipitacao",
  "completude_por_fonte",
  "lista_curta_candidatos_ate20km.csv"
)

ARQ_MP10 <- file.path(
  "outputs", "04_completude",
  "completude_diaria_mp10.csv"
)

SAIDA <- file.path(
  "outputs", "09_meteorologia", "precipitacao",
  "series_diarias_candidatos"
)

dir.create(SAIDA, recursive = TRUE, showWarnings = FALSE)

for (f in c(ARQ_QAQC, ARQ_COMP_DIA, ARQ_LISTA, ARQ_MP10)) {
  if (!file.exists(f)) stop("Arquivo necessario nao encontrado: ", f)
}

cor_segura <- function(x, y, metodo = "pearson") {
  ok <- is.finite(x) & is.finite(y)
  if (sum(ok) < 10) return(NA_real_)
  if (sd(x[ok]) == 0 || sd(y[ok]) == 0) return(NA_real_)
  suppressWarnings(cor(x[ok], y[ok], method = metodo))
}

rmse <- function(x) {
  x <- x[is.finite(x)]
  if (!length(x)) return(NA_real_)
  sqrt(mean(x^2))
}

mae <- function(x) {
  x <- x[is.finite(x)]
  if (!length(x)) return(NA_real_)
  mean(abs(x))
}

mediana_abs <- function(x) {
  x <- x[is.finite(x)]
  if (!length(x)) return(NA_real_)
  median(abs(x))
}

jaccard_binario <- function(a, b) {
  a <- a %in% TRUE
  b <- b %in% TRUE
  uniao <- sum(a | b)
  if (uniao == 0) return(NA_real_)
  sum(a & b) / uniao
}

metricas_par <- function(d, flag_a, flag_b, criterio) {

  cobertura_a <- d[[flag_a]] %in% TRUE
  cobertura_b <- d[[flag_b]] %in% TRUE
  comum <- cobertura_a & cobertura_b

  x <- d$precip_a
  y <- d$precip_b

  ok <- comum & is.finite(x) & is.finite(y)
  ok_mp10 <- ok & (d$dia_mp10_valido %in% TRUE)

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

    media_a_mm = if (sum(ok)) mean(x[ok]) else NA_real_,
    media_b_mm = if (sum(ok)) mean(y[ok]) else NA_real_,
    total_a_comum_mm = if (sum(ok)) sum(x[ok]) else NA_real_,
    total_b_comum_mm = if (sum(ok)) sum(y[ok]) else NA_real_,
    razao_total_b_a = if (
      sum(ok) && sum(x[ok]) > 0
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
    mediana_abs_diff_mm = mediana_abs(dif),

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

cat("\nCarregando candidatos da Etapa 9D...\n")

lista <- fread(
  ARQ_LISTA,
  encoding = "UTF-8"
)

lista[, cod_estacao := as.character(cod_estacao)]
lista[, ano_alvo := as.integer(ano_alvo)]

cand <- unique(
  lista[
    fonte_dados == "CEMADEN" &
      elegivel_pareamento_primario == TRUE,
    .(
      estacao_historica,
      ano_alvo,
      cod_estacao,
      nome_estacao,
      municipios,
      distancia_km,
      pct_dias_precip_com_valor,
      pct_overlap_primario,
      pct_overlap_sens_1,
      pct_overlap_sens_2
    )
  ]
)

if (!nrow(cand)) {
  stop("Nenhum candidato CEMADEN elegivel encontrado na Etapa 9D.")
}

cat("Carregando base QA/QC e agregando precipitacao diaria...\n")

p <- readRDS(ARQ_QAQC)
setDT(p)

p[, cod_estacao := as.character(cod_estacao)]
p[, ano := as.integer(ano)]

chaves <- unique(
  cand[
    ,
    .(
      cod_estacao,
      ano = ano_alvo
    )
  ]
)

p <- p[
  fonte_dados == "CEMADEN" &
    valor_valido == TRUE
][
  chaves,
  on = .(
    cod_estacao,
    ano
  ),
  nomatch = 0
]

if (!nrow(p)) {
  stop("Nenhum registro valido localizado para os candidatos.")
}

chuva_dia <- p[
  ,
  .(
    precip_diaria_mm = sum(
      precipitacao_mm,
      na.rm = TRUE
    ),
    n_registros_validos = .N,
    max_acumulado_intervalo_mm = max(
      precipitacao_mm,
      na.rm = TRUE
    ),
    n_registros_chuva = sum(
      precipitacao_mm > 0,
      na.rm = TRUE
    )
  ),
  by = .(
    cod_estacao,
    ano,
    data_local
  )
]

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
  data_intervalo_sens := as.IDate(
    data_local
  )
]

p[
  hora_local_txt == "00:00:00",
  data_intervalo_sens := data_intervalo_sens - 1L
]

chuva_dia_sens <- p[
  ,
  .(
    precip_diaria_mm_sens_midnight = sum(
      precipitacao_mm,
      na.rm = TRUE
    )
  ),
  by = .(
    cod_estacao,
    ano,
    data_local = data_intervalo_sens
  )
]

rm(p)
gc()

chuva_dia <- merge(
  chuva_dia,
  chuva_dia_sens,
  by = c(
    "cod_estacao",
    "ano",
    "data_local"
  ),
  all = TRUE
)

cat("Integrando flags de continuidade da Etapa 9D...\n")

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
  fonte_dados == "CEMADEN",
  .(
    cod_estacao,
    ano,
    data_local,
    n_obs_validas,
    maior_gap_dia_min,
    cobertura_60 = cobertura_primaria,
    cobertura_70 = cobertura_sens_1,
    cobertura_90 = cobertura_sens_2
  )
]

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
    estacao_historica = as.character(
      no_estacao
    )
  )
]

mp10 <- mp10[
  ,
  .(
    estacao_historica,
    ano_alvo = ano,
    data_local,
    dia_mp10_valido = dia_valido_mma_16h,
    media_mp10 = media_24h_mma
  )
]

cat("Construindo series diarias completas dos candidatos...\n")

grade <- cand[
  ,
  .(
    data_local = seq(
      as.IDate(
        sprintf(
          "%04d-01-01",
          ano_alvo
        )
      ),
      as.IDate(
        sprintf(
          "%04d-12-31",
          ano_alvo
        )
      ),
      by = "day"
    )
  ),
  by = .(
    estacao_historica,
    ano_alvo,
    cod_estacao,
    nome_estacao,
    municipios,
    distancia_km,
    pct_dias_precip_com_valor,
    pct_overlap_primario,
    pct_overlap_sens_1,
    pct_overlap_sens_2
  )
]

serie <- merge(
  grade,
  chuva_dia[
    ,
    .(
      cod_estacao,
      ano_alvo = ano,
      data_local,
      precip_diaria_mm,
      precip_diaria_mm_sens_midnight,
      n_registros_validos,
      n_registros_chuva,
      max_acumulado_intervalo_mm
    )
  ],
  by = c(
    "cod_estacao",
    "ano_alvo",
    "data_local"
  ),
  all.x = TRUE
)

serie <- merge(
  serie,
  comp[
    ,
    .(
      cod_estacao,
      ano_alvo = ano,
      data_local,
      n_obs_validas,
      maior_gap_dia_min,
      cobertura_60,
      cobertura_70,
      cobertura_90
    )
  ],
  by = c(
    "cod_estacao",
    "ano_alvo",
    "data_local"
  ),
  all.x = TRUE
)

serie <- merge(
  serie,
  mp10,
  by = c(
    "estacao_historica",
    "ano_alvo",
    "data_local"
  ),
  all.x = TRUE
)

serie[
  ,
  tem_precipitacao := !is.na(
    precip_diaria_mm
  )
]

serie[
  ,
  diferenca_midnight_mm :=
    precip_diaria_mm_sens_midnight -
    precip_diaria_mm
]

setorder(
  serie,
  estacao_historica,
  ano_alvo,
  distancia_km,
  cod_estacao,
  data_local
)

fwrite(
  serie,
  file.path(
    SAIDA,
    "series_diarias_candidatos_precipitacao.csv"
  ),
  bom = TRUE
)

sens_midnight <- serie[
  tem_precipitacao == TRUE &
    is.finite(
      diferenca_midnight_mm
    ),
  .(
    n_dias_com_diferenca = sum(
      abs(
        diferenca_midnight_mm
      ) > 1e-12
    ),
    maior_abs_diferenca_mm = max(
      abs(
        diferenca_midnight_mm
      ),
      na.rm = TRUE
    ),
    soma_abs_diferencas_mm = sum(
      abs(
        diferenca_midnight_mm
      ),
      na.rm = TRUE
    )
  ),
  by = .(
    cod_estacao,
    nome_estacao,
    ano_alvo
  )
]

fwrite(
  sens_midnight,
  file.path(
    SAIDA,
    "sensibilidade_atribuicao_meia_noite.csv"
  ),
  bom = TRUE
)

resumo_cand <- serie[
  ,
  .(
    distancia_km = unique(
      distancia_km
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
    }
  ),
  by = .(
    estacao_historica,
    ano_alvo,
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
  estacao_historica,
  ano_alvo,
  distancia_km
)

fwrite(
  resumo_cand,
  file.path(
    SAIDA,
    "resumo_series_diarias_candidatos.csv"
  ),
  bom = TRUE
)

cat("Comparando pares de pluviometros...\n")

alvos <- unique(
  cand[
    ,
    .(
      estacao_historica,
      ano_alvo
    )
  ]
)

resultados_pares <- list()
idx <- 0L

for (ii in seq_len(nrow(alvos))) {

  alvo_est <- alvos$estacao_historica[ii]
  alvo_ano <- alvos$ano_alvo[ii]

  cc <- cand[
    estacao_historica == alvo_est &
      ano_alvo == alvo_ano
  ][
    order(
      distancia_km,
      cod_estacao
    )
  ]

  if (nrow(cc) < 2) next

  combinacoes <- combn(
    seq_len(
      nrow(cc)
    ),
    2
  )

  for (jj in seq_len(ncol(combinacoes))) {

    ia <- combinacoes[1, jj]
    ib <- combinacoes[2, jj]

    ca <- cc[ia]
    cb <- cc[ib]

    sa <- serie[
      estacao_historica == alvo_est &
        ano_alvo == alvo_ano &
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

    sb <- serie[
      estacao_historica == alvo_est &
        ano_alvo == alvo_ano &
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
      sa,
      sb,
      by = "data_local",
      all = TRUE
    )

    metas <- rbindlist(
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

    metas[
      ,
      `:=`(
        estacao_historica = alvo_est,
        ano_alvo = alvo_ano,
        cod_estacao_a = ca$cod_estacao,
        nome_estacao_a = ca$nome_estacao,
        distancia_a_km = ca$distancia_km,
        cod_estacao_b = cb$cod_estacao,
        nome_estacao_b = cb$nome_estacao,
        distancia_b_km = cb$distancia_km
      )
    ]

    idx <- idx + 1L
    resultados_pares[[idx]] <- metas
  }
}

pares <- if (
  length(
    resultados_pares
  )
) {
  rbindlist(
    resultados_pares,
    fill = TRUE
  )
} else {
  data.table()
}

if (nrow(pares)) {
  setorder(
    pares,
    estacao_historica,
    ano_alvo,
    criterio,
    distancia_a_km,
    distancia_b_km
  )
}

fwrite(
  pares,
  file.path(
    SAIDA,
    "comparacao_pareada_pluviometros.csv"
  ),
  bom = TRUE
)

codigos_nucleo <- c(
  "260720801A",
  "260720804A",
  "260720801G"
)

nucleo <- pares[
  cod_estacao_a %in% codigos_nucleo &
    cod_estacao_b %in% codigos_nucleo
]

fwrite(
  nucleo,
  file.path(
    SAIDA,
    "comparacao_nucleo_local_ipojuca.csv"
  ),
  bom = TRUE
)

resumo_exec <- data.table(
  indicador = c(
    "n_alvos_mp10",
    "n_combinacoes_candidato_alvo",
    "n_codigos_cemaden_distintos",
    "n_linhas_series_diarias",
    "n_comparacoes_pareadas_criterio",
    "n_comparacoes_nucleo_ipojuca",
    "n_candidatos_com_sensibilidade_midnight"
  ),
  valor = c(
    nrow(alvos),
    nrow(cand),
    uniqueN(cand$cod_estacao),
    nrow(serie),
    nrow(pares),
    nrow(nucleo),
    sum(
      sens_midnight$n_dias_com_diferenca > 0,
      na.rm = TRUE
    )
  )
)

fwrite(
  resumo_exec,
  file.path(
    SAIDA,
    "resumo_execucao_series_diarias.csv"
  ),
  bom = TRUE
)

cat("\n============================================================\n")
cat("ETAPA 9E - SERIES DIARIAS E COERENCIA CONCLUIDA\n")
cat("============================================================\n\n")

print(resumo_exec)

cat("\nCandidatos por alvo:\n")
print(
  resumo_cand[
    ,
    .(
      estacao_historica,
      ano_alvo,
      cod_estacao,
      nome_estacao,
      distancia_km,
      pct_overlap_60_mp10,
      pct_overlap_70_mp10,
      pct_overlap_90_mp10,
      max_precip_diaria_mm
    )
  ]
)

cat("\nSaidas em:\n")
cat(SAIDA, "\n")

cat(
  "\nIMPORTANTE:\n",
  "- os totais diarios usam apenas registros previamente considerados validos na 9C;\n",
  "- dias incompletos permanecem na tabela, mas sao explicitamente flagados;\n",
  "- nenhuma estacao foi escolhida automaticamente;\n",
  "- a decisao final deve combinar proximidade, cobertura e coerencia entre series.\n",
  sep = ""
)
