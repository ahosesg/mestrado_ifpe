# ============================================================
# ETAPA 10 - CARACTERIZACAO TEMPORAL PRELIMINAR DO MP10
# Projeto de Mestrado - MP10 / Suape
# ============================================================
#
# OBJETIVO
# Caracterizar a variabilidade temporal das concentracoes diarias
# de MP10 nos station-years que possuem referencia anual valida.
#
# ESCOPO
# - IFPE 2018
# - Gaibu 2019
# - IPOJUCA 2021
# - CUPE 2021
# - SUAPE 2022
# - IPOJUCA 2025
#
# PRINCIPIOS
# - usar somente medias diarias validas segundo o criterio principal
#   da Etapa 4 (>=16 horas validas);
# - preservar todos os dias do calendario para autocorrelacao e
#   identificacao de lacunas;
# - meses nao representativos permanecem descritos, mas sao
#   explicitamente sinalizados e nao tratados como equivalentes aos
#   meses representativos;
# - a referencia anual congelada na Etapa 5 e usada apenas como
#   referencia comparativa, sem recalculo;
# - nao realizar ainda atribuicao causal meteorologica;
# - nao aplicar remocao adicional de extremos.
# ============================================================

if (!requireNamespace("data.table", quietly = TRUE)) {
  stop("Instale o pacote 'data.table' antes de executar.")
}
library(data.table)

# ------------------------------------------------------------
# 1. CAMINHOS
# ------------------------------------------------------------

ARQ_DIARIO <- file.path(
  "outputs", "04_completude",
  "completude_diaria_mp10.csv"
)

ARQ_MENSAL <- file.path(
  "outputs", "04_completude",
  "completude_mensal_mp10.csv"
)

ARQ_REFS <- file.path(
  "outputs", "05_referencias_anuais",
  "referencias_anuais_elegiveis_principal.csv"
)

SAIDA <- file.path(
  "outputs", "10_caracterizacao_temporal"
)

dir.create(
  SAIDA,
  recursive = TRUE,
  showWarnings = FALSE
)

for (f in c(
  ARQ_DIARIO,
  ARQ_MENSAL,
  ARQ_REFS
)) {
  if (!file.exists(f)) {
    stop(
      "Arquivo necessario nao encontrado: ",
      f
    )
  }
}

# ------------------------------------------------------------
# 2. FUNCOES AUXILIARES
# ------------------------------------------------------------

media_segura <- function(x) {
  z <- x[is.finite(x)]
  if (!length(z)) return(NA_real_)
  mean(z)
}

sd_seguro <- function(x) {
  z <- x[is.finite(x)]
  if (length(z) < 2L) return(NA_real_)
  sd(z)
}

quantil_seguro <- function(x, p) {
  z <- x[is.finite(x)]
  if (!length(z)) return(NA_real_)
  as.numeric(
    quantile(
      z,
      probs = p,
      names = FALSE,
      type = 7
    )
  )
}

cv_seguro <- function(x) {
  m <- media_segura(x)
  s <- sd_seguro(x)
  if (
    !is.finite(m) ||
      m == 0 ||
      !is.finite(s)
  ) {
    return(NA_real_)
  }
  100 * s / m
}

cor_segura <- function(x, y, metodo = "pearson") {
  ok <- is.finite(x) & is.finite(y)
  if (sum(ok) < 10L) return(NA_real_)
  if (
    sd(x[ok]) == 0 ||
      sd(y[ok]) == 0
  ) {
    return(NA_real_)
  }
  suppressWarnings(
    cor(
      x[ok],
      y[ok],
      method = metodo
    )
  )
}

mae_seguro <- function(x) {
  z <- x[is.finite(x)]
  if (!length(z)) return(NA_real_)
  mean(abs(z))
}

rmse_seguro <- function(x) {
  z <- x[is.finite(x)]
  if (!length(z)) return(NA_real_)
  sqrt(mean(z^2))
}

max_run_true <- function(x) {
  if (!length(x)) return(0L)
  x[is.na(x)] <- FALSE
  if (!any(x)) return(0L)

  rr <- rle(x)
  max(
    rr$lengths[
      rr$values
    ]
  )
}

autocor_lag_calendario <- function(x, lag_dias) {
  n <- length(x)

  if (n <= lag_dias) {
    return(
      data.table(
        lag_dias = lag_dias,
        n_pares = 0L,
        autocorrelacao = NA_real_
      )
    )
  }

  a <- x[
    seq_len(
      n - lag_dias
    )
  ]

  b <- x[
    seq.int(
      1L + lag_dias,
      n
    )
  ]

  ok <- is.finite(a) & is.finite(b)

  if (sum(ok) < 10L) {
    return(
      data.table(
        lag_dias = lag_dias,
        n_pares = sum(ok),
        autocorrelacao = NA_real_
      )
    )
  }

  if (
    sd(a[ok]) == 0 ||
      sd(b[ok]) == 0
  ) {
    r <- NA_real_
  } else {
    r <- cor(
      a[ok],
      b[ok],
      method = "pearson"
    )
  }

  data.table(
    lag_dias = lag_dias,
    n_pares = sum(ok),
    autocorrelacao = r
  )
}

quadrimestre <- function(mes) {
  fifelse(
    mes <= 4L,
    1L,
    fifelse(
      mes <= 8L,
      2L,
      3L
    )
  )
}

nome_mes_pt <- function(mes) {
  nomes <- c(
    "Jan", "Fev", "Mar", "Abr",
    "Mai", "Jun", "Jul", "Ago",
    "Set", "Out", "Nov", "Dez"
  )
  nomes[mes]
}

# ------------------------------------------------------------
# 3. CARREGAR E NORMALIZAR
# ------------------------------------------------------------

cat("\nCarregando medias diarias e referencias anuais...\n")

diario <- fread(
  ARQ_DIARIO,
  encoding = "UTF-8"
)

mensal_et4 <- fread(
  ARQ_MENSAL,
  encoding = "UTF-8"
)

refs <- fread(
  ARQ_REFS,
  encoding = "UTF-8"
)

diario[
  ,
  `:=`(
    ano = as.integer(ano),
    cod_estacao = as.character(cod_estacao),
    no_estacao = as.character(no_estacao),
    data = as.IDate(data),
    mes = as.integer(mes)
  )
]

mensal_et4[
  ,
  `:=`(
    ano = as.integer(ano),
    cod_estacao = as.character(cod_estacao),
    no_estacao = as.character(no_estacao),
    mes = as.integer(mes)
  )
]

refs[
  ,
  `:=`(
    ano = as.integer(ano),
    cod_estacao = as.character(cod_estacao),
    no_estacao = as.character(no_estacao),
    referencia_anual_principal =
      as.numeric(
        referencia_anual_principal
      )
  )
]

# ------------------------------------------------------------
# 4. SELECIONAR SOMENTE OS ANOS DE REFERENCIA VALIDOS
# ------------------------------------------------------------

alvos <- unique(
  refs[
    ,
    .(
      ano,
      cod_estacao,
      no_estacao,
      referencia_anual_principal,
      n_dias_ref_16h,
      menor_pct_quad_16h
    )
  ]
)

base <- merge(
  diario,
  alvos,
  by = c(
    "ano",
    "cod_estacao",
    "no_estacao"
  ),
  all = FALSE
)

base[
  ,
  mp10_diario := fifelse(
    dia_valido_mma_16h %in% TRUE,
    as.numeric(media_24h_mma),
    NA_real_
  )
]

base[
  ,
  `:=`(
    dia_do_ano =
      as.integer(
        format(
          data,
          "%j"
        )
      ),
    mes_nome =
      nome_mes_pt(mes),
    quadrimestre =
      quadrimestre(mes),
    dia_semana =
      weekdays(
        as.Date(data)
      ),
    acima_referencia =
      is.finite(mp10_diario) &
        mp10_diario >
          referencia_anual_principal
  )
]

setorder(
  base,
  ano,
  no_estacao,
  data
)

# Auditoria: cada alvo deve ter o calendario inteiro.
aud_grade <- base[
  ,
  .(
    n_dias_calendario = .N,
    n_datas_unicas =
      uniqueN(data),
    primeira_data =
      min(data),
    ultima_data =
      max(data),
    n_dias_validos =
      sum(
        is.finite(
          mp10_diario
        )
      )
  ),
  by = .(
    ano,
    cod_estacao,
    no_estacao
  )
]

aud_grade[
  ,
  grade_ok := (
    n_dias_calendario %in%
      c(365L, 366L) &
      n_dias_calendario ==
        n_datas_unicas
  )
]

fwrite(
  aud_grade,
  file.path(
    SAIDA,
    "auditoria_grade_diaria_referencias.csv"
  ),
  bom = TRUE
)

if (any(!aud_grade$grade_ok)) {
  stop(
    "Uma ou mais series de referencia nao possuem grade diaria completa."
  )
}

# ------------------------------------------------------------
# 5. SERIE DIARIA CONSOLIDADA
# ------------------------------------------------------------

base_export <- base[
  ,
  .(
    ano,
    cod_estacao,
    no_estacao,
    data,
    dia_do_ano,
    mes,
    mes_nome,
    quadrimestre,
    dia_semana,
    n_horas_validas,
    dia_valido_mma_16h,
    mp10_diario,
    referencia_anual_principal,
    acima_referencia
  )
]

fwrite(
  base_export,
  file.path(
    SAIDA,
    "series_diarias_mp10_referencias_validas.csv"
  ),
  bom = TRUE
)

# ------------------------------------------------------------
# 6. ESTATISTICAS DESCRITIVAS POR STATION-YEAR
# ------------------------------------------------------------

cat("Calculando estatisticas descritivas anuais...\n")

desc_anual <- base[
  ,
  {
    x <- mp10_diario[
      is.finite(
        mp10_diario
      )
    ]

    ref <- unique(
      referencia_anual_principal
    )[1]

    list(
      n_dias_calendario = .N,
      n_dias_validos =
        length(x),
      pct_dias_validos =
        100 * length(x) / .N,

      media =
        media_segura(x),
      mediana =
        quantil_seguro(x, 0.50),
      desvio_padrao =
        sd_seguro(x),
      cv_pct =
        cv_seguro(x),

      minimo =
        if (length(x)) {
          min(x)
        } else {
          NA_real_
        },

      p05 =
        quantil_seguro(x, 0.05),
      p25 =
        quantil_seguro(x, 0.25),
      p75 =
        quantil_seguro(x, 0.75),
      p95 =
        quantil_seguro(x, 0.95),
      p99 =
        quantil_seguro(x, 0.99),

      maximo =
        if (length(x)) {
          max(x)
        } else {
          NA_real_
        },

      iqr =
        quantil_seguro(x, 0.75) -
          quantil_seguro(x, 0.25),

      referencia_anual =
        ref,

      diferenca_media_vs_ref =
        media_segura(x) -
          ref,

      n_dias_acima_ref =
        sum(
          x > ref
        ),

      pct_dias_acima_ref =
        if (length(x)) {
          100 *
            sum(
              x > ref
            ) /
            length(x)
        } else {
          NA_real_
        },

      maior_lacuna_dias_sem_media_valida =
        max_run_true(
          !is.finite(
            mp10_diario
          )
        )
    )
  },
  by = .(
    ano,
    cod_estacao,
    no_estacao
  )
]

fwrite(
  desc_anual,
  file.path(
    SAIDA,
    "estatisticas_descritivas_anuais_mp10.csv"
  ),
  bom = TRUE
)

# ------------------------------------------------------------
# 7. ESTATISTICAS MENSAIS
# ------------------------------------------------------------

cat("Calculando variabilidade mensal...\n")

mensal <- base[
  ,
  {
    x <- mp10_diario[
      is.finite(
        mp10_diario
      )
    ]

    ref <- unique(
      referencia_anual_principal
    )[1]

    list(
      n_dias_calendario = .N,
      n_dias_validos =
        length(x),
      pct_dias_validos =
        100 * length(x) / .N,

      media_mensal_descritiva =
        media_segura(x),
      mediana_mensal =
        quantil_seguro(x, 0.50),
      desvio_padrao_mensal =
        sd_seguro(x),
      cv_mensal_pct =
        cv_seguro(x),
      p25 =
        quantil_seguro(x, 0.25),
      p75 =
        quantil_seguro(x, 0.75),
      p95 =
        quantil_seguro(x, 0.95),

      referencia_anual =
        ref,

      diferenca_mensal_vs_ref =
        media_segura(x) -
          ref,

      diferenca_mensal_vs_ref_pct =
        if (
          is.finite(ref) &&
            ref != 0
        ) {
          100 *
            (
              media_segura(x) -
                ref
            ) /
            ref
        } else {
          NA_real_
        }
    )
  },
  by = .(
    ano,
    cod_estacao,
    no_estacao,
    mes,
    mes_nome
  )
]

mensal <- merge(
  mensal,
  mensal_et4[
    ,
    .(
      ano,
      cod_estacao,
      no_estacao,
      mes,
      minimo_dias_mma,
      mes_representativo_mma
    )
  ],
  by = c(
    "ano",
    "cod_estacao",
    "no_estacao",
    "mes"
  ),
  all.x = TRUE
)

setorder(
  mensal,
  ano,
  no_estacao,
  mes
)

fwrite(
  mensal,
  file.path(
    SAIDA,
    "estatisticas_mensais_mp10.csv"
  ),
  bom = TRUE
)

# ------------------------------------------------------------
# 8. VARIABILIDADE SAZONAL POR QUADRIMESTRE
# ------------------------------------------------------------

quad <- base[
  ,
  {
    x <- mp10_diario[
      is.finite(
        mp10_diario
      )
    ]

    ref <- unique(
      referencia_anual_principal
    )[1]

    list(
      n_dias_calendario = .N,
      n_dias_validos =
        length(x),
      pct_dias_validos =
        100 * length(x) / .N,
      media_quadrimestral =
        media_segura(x),
      mediana_quadrimestral =
        quantil_seguro(x, 0.50),
      desvio_padrao_quadrimestral =
        sd_seguro(x),
      cv_quadrimestral_pct =
        cv_seguro(x),
      p95_quadrimestral =
        quantil_seguro(x, 0.95),
      referencia_anual =
        ref,
      diferenca_vs_ref_pct =
        if (
          is.finite(ref) &&
            ref != 0
        ) {
          100 *
            (
              media_segura(x) -
                ref
            ) /
            ref
        } else {
          NA_real_
        }
    )
  },
  by = .(
    ano,
    cod_estacao,
    no_estacao,
    quadrimestre
  )
]

fwrite(
  quad,
  file.path(
    SAIDA,
    "estatisticas_quadrimestrais_mp10.csv"
  ),
  bom = TRUE
)

# Amplitude entre quadrimestres.
amp_quad <- quad[
  ,
  .(
    media_quadrimestral_min =
      min(
        media_quadrimestral,
        na.rm = TRUE
      ),
    media_quadrimestral_max =
      max(
        media_quadrimestral,
        na.rm = TRUE
      ),
    amplitude_quadrimestral =
      max(
        media_quadrimestral,
        na.rm = TRUE
      ) -
        min(
          media_quadrimestral,
          na.rm = TRUE
        ),
    razao_max_min =
      if (
        min(
          media_quadrimestral,
          na.rm = TRUE
        ) > 0
      ) {
        max(
          media_quadrimestral,
          na.rm = TRUE
        ) /
          min(
            media_quadrimestral,
            na.rm = TRUE
          )
      } else {
        NA_real_
      }
  ),
  by = .(
    ano,
    cod_estacao,
    no_estacao
  )
]

fwrite(
  amp_quad,
  file.path(
    SAIDA,
    "amplitude_quadrimestral_mp10.csv"
  ),
  bom = TRUE
)

# ------------------------------------------------------------
# 9. AUTOCORRELACAO EM LAGS FIXOS DE CALENDARIO
# ------------------------------------------------------------

cat("Calculando autocorrelacao temporal...\n")

lista_acf <- list()
k_acf <- 0L

chaves <- unique(
  base[
    ,
    .(
      ano,
      cod_estacao,
      no_estacao
    )
  ]
)

for (i in seq_len(nrow(chaves))) {

  a <- chaves$ano[i]
  codi <- chaves$cod_estacao[i]
  nome <- chaves$no_estacao[i]

  z <- base[
    ano == a &
      cod_estacao == codi &
      no_estacao == nome
  ][
    order(data)
  ]

  for (lag_dias in c(
    1L,
    2L,
    3L,
    7L,
    14L,
    30L
  )) {

    tmp <- autocor_lag_calendario(
      z$mp10_diario,
      lag_dias
    )

    tmp[
      ,
      `:=`(
        ano = a,
        cod_estacao = codi,
        no_estacao = nome
      )
    ]

    k_acf <- k_acf + 1L
    lista_acf[[k_acf]] <- tmp
  }
}

acf_res <- rbindlist(
  lista_acf,
  fill = TRUE
)

setcolorder(
  acf_res,
  c(
    "ano",
    "cod_estacao",
    "no_estacao",
    "lag_dias",
    "n_pares",
    "autocorrelacao"
  )
)

fwrite(
  acf_res,
  file.path(
    SAIDA,
    "autocorrelacao_temporal_mp10.csv"
  ),
  bom = TRUE
)

# ------------------------------------------------------------
# 10. COMPARACAO ESPACIAL CONTEMPORANEA, CUPE X IPOJUCA 2021
# ------------------------------------------------------------

cat("Comparando series contemporaneas de 2021...\n")

cupe <- base[
  ano == 2021L &
    no_estacao == "CUPE",
  .(
    data,
    mp10_cupe =
      mp10_diario
  )
]

ipoj <- base[
  ano == 2021L &
    no_estacao == "IPOJUCA",
  .(
    data,
    mp10_ipojuca =
      mp10_diario
  )
]

par_2021 <- merge(
  cupe,
  ipoj,
  by = "data",
  all = TRUE
)

par_2021[
  ,
  ambos_validos := (
    is.finite(
      mp10_cupe
    ) &
      is.finite(
        mp10_ipojuca
      )
  )
]

par_2021[
  ,
  diferenca_ipojuca_menos_cupe :=
    mp10_ipojuca -
      mp10_cupe
]

fwrite(
  par_2021,
  file.path(
    SAIDA,
    "serie_pareada_cupe_ipojuca_2021.csv"
  ),
  bom = TRUE
)

ok_2021 <- par_2021$ambos_validos %in% TRUE

comp_2021 <- data.table(
  ano = 2021L,
  estacao_a = "CUPE",
  estacao_b = "IPOJUCA",
  n_dias_comuns =
    sum(ok_2021),
  correlacao_pearson =
    cor_segura(
      par_2021$mp10_cupe,
      par_2021$mp10_ipojuca,
      "pearson"
    ),
  correlacao_spearman =
    cor_segura(
      par_2021$mp10_cupe,
      par_2021$mp10_ipojuca,
      "spearman"
    ),
  vies_ipojuca_menos_cupe =
    media_segura(
      par_2021$diferenca_ipojuca_menos_cupe[
        ok_2021
      ]
    ),
  mae =
    mae_seguro(
      par_2021$diferenca_ipojuca_menos_cupe[
        ok_2021
      ]
    ),
  rmse =
    rmse_seguro(
      par_2021$diferenca_ipojuca_menos_cupe[
        ok_2021
      ]
    ),
  razao_medias_ipojuca_cupe =
    if (
      media_segura(
        par_2021$mp10_cupe[
          ok_2021
        ]
      ) > 0
    ) {
      media_segura(
        par_2021$mp10_ipojuca[
          ok_2021
        ]
      ) /
        media_segura(
          par_2021$mp10_cupe[
            ok_2021
          ]
        )
    } else {
      NA_real_
    }
)

fwrite(
  comp_2021,
  file.path(
    SAIDA,
    "comparacao_cupe_ipojuca_2021.csv"
  ),
  bom = TRUE
)

# ------------------------------------------------------------
# 11. MESES EXTREMOS, APENAS ENTRE MESES REPRESENTATIVOS
# ------------------------------------------------------------

meses_rep <- mensal[
  mes_representativo_mma %in% TRUE &
    is.finite(
      media_mensal_descritiva
    )
]

extremos_mensais <- meses_rep[
  ,
  {
    i_min <- which.min(
      media_mensal_descritiva
    )
    i_max <- which.max(
      media_mensal_descritiva
    )

    list(
      n_meses_representativos = .N,
      mes_menor_media =
        mes[i_min],
      nome_mes_menor_media =
        mes_nome[i_min],
      menor_media_mensal =
        media_mensal_descritiva[i_min],
      mes_maior_media =
        mes[i_max],
      nome_mes_maior_media =
        mes_nome[i_max],
      maior_media_mensal =
        media_mensal_descritiva[i_max],
      amplitude_mensal =
        media_mensal_descritiva[i_max] -
          media_mensal_descritiva[i_min]
    )
  },
  by = .(
    ano,
    cod_estacao,
    no_estacao
  )
]

fwrite(
  extremos_mensais,
  file.path(
    SAIDA,
    "extremos_mensais_representativos_mp10.csv"
  ),
  bom = TRUE
)

# ------------------------------------------------------------
# 12. GRAFICOS PRELIMINARES
# ------------------------------------------------------------

cat("Gerando graficos preliminares...\n")

# 12.1 Series diarias por station-year.
png(
  file.path(
    SAIDA,
    "painel_series_diarias_mp10.png"
  ),
  width = 1800,
  height = 1500,
  res = 180
)

par(
  mfrow = c(3, 2),
  mar = c(4, 4, 3, 1)
)

for (i in seq_len(nrow(chaves))) {

  a <- chaves$ano[i]
  codi <- chaves$cod_estacao[i]
  nome <- chaves$no_estacao[i]

  z <- base[
    ano == a &
      cod_estacao == codi &
      no_estacao == nome
  ][
    order(data)
  ]

  plot(
    as.Date(z$data),
    z$mp10_diario,
    type = "l",
    xlab = "Data",
    ylab = expression(
      MP[10]~(mu*g/m^3)
    ),
    main = paste(
      nome,
      a
    )
  )

  abline(
    h = unique(
      z$referencia_anual_principal
    )[1],
    lty = 2
  )
}

dev.off()

# 12.2 Medias mensais descritivas, marcando meses nao representativos.
png(
  file.path(
    SAIDA,
    "painel_medias_mensais_mp10.png"
  ),
  width = 1800,
  height = 1500,
  res = 180
)

par(
  mfrow = c(3, 2),
  mar = c(4, 4, 3, 1)
)

for (i in seq_len(nrow(chaves))) {

  a <- chaves$ano[i]
  codi <- chaves$cod_estacao[i]
  nome <- chaves$no_estacao[i]

  z <- mensal[
    ano == a &
      cod_estacao == codi &
      no_estacao == nome
  ][
    order(mes)
  ]

  ylim_sup <- max(
    c(
      z$media_mensal_descritiva,
      z$referencia_anual
    ),
    na.rm = TRUE
  )

  plot(
    z$mes,
    z$media_mensal_descritiva,
    type = "b",
    xaxt = "n",
    xlab = "Mes",
    ylab = expression(
      MP[10]~(mu*g/m^3)
    ),
    ylim = c(0, ylim_sup * 1.15),
    main = paste(
      nome,
      a
    )
  )

  axis(
    1,
    at = 1:12,
    labels = nome_mes_pt(
      1:12
    )
  )

  abline(
    h = unique(
      z$referencia_anual
    )[1],
    lty = 2
  )

  idx_nao_rep <- which(
    !(z$mes_representativo_mma %in% TRUE) &
      is.finite(
        z$media_mensal_descritiva
      )
  )

  if (length(idx_nao_rep)) {
    points(
      z$mes[idx_nao_rep],
      z$media_mensal_descritiva[idx_nao_rep],
      pch = 4,
      cex = 1.4
    )
  }
}

dev.off()

# 12.3 Boxplots mensais por station-year.
png(
  file.path(
    SAIDA,
    "painel_distribuicao_mensal_mp10.png"
  ),
  width = 1800,
  height = 1500,
  res = 180
)

par(
  mfrow = c(3, 2),
  mar = c(4, 4, 3, 1)
)

for (i in seq_len(nrow(chaves))) {

  a <- chaves$ano[i]
  codi <- chaves$cod_estacao[i]
  nome <- chaves$no_estacao[i]

  z <- base[
    ano == a &
      cod_estacao == codi &
      no_estacao == nome &
      is.finite(
        mp10_diario
      )
  ]

  boxplot(
    mp10_diario ~ mes,
    data = z,
    names = nome_mes_pt(
      sort(
        unique(
          z$mes
        )
      )
    ),
    xlab = "Mes",
    ylab = expression(
      MP[10]~(mu*g/m^3)
    ),
    main = paste(
      nome,
      a
    ),
    outline = TRUE
  )
}

dev.off()

# ------------------------------------------------------------
# 13. RESUMO EXECUCAO
# ------------------------------------------------------------

resumo_exec <- data.table(
  indicador = c(
    "n_station_years_analisados",
    "n_dias_calendario_total",
    "n_dias_validos_total",
    "n_series_com_12_meses_representativos",
    "n_series_com_autocorrelacao_calculada",
    "n_comparacoes_espaciais_contemporaneas",
    "n_graficos_gerados"
  ),
  valor = c(
    nrow(chaves),
    nrow(base),
    sum(
      is.finite(
        base$mp10_diario
      )
    ),
    sum(
      extremos_mensais$n_meses_representativos ==
        12L
    ),
    uniqueN(
      acf_res[
        is.finite(
          autocorrelacao
        ),
        paste(
          ano,
          cod_estacao
        )
      ]
    ),
    1L,
    3L
  )
)

fwrite(
  resumo_exec,
  file.path(
    SAIDA,
    "resumo_execucao_caracterizacao_temporal.csv"
  ),
  bom = TRUE
)

# ------------------------------------------------------------
# 14. CONSOLE
# ------------------------------------------------------------

cat("\n============================================================\n")
cat("ETAPA 10 - CARACTERIZACAO TEMPORAL PRELIMINAR CONCLUIDA\n")
cat("============================================================\n\n")

print(resumo_exec)

cat("\nEstatisticas anuais:\n")

print(
  desc_anual[
    ,
    .(
      no_estacao,
      ano,
      n_dias_validos,
      pct_dias_validos,
      media,
      mediana,
      desvio_padrao,
      cv_pct,
      p95,
      maximo,
      referencia_anual,
      pct_dias_acima_ref,
      maior_lacuna_dias_sem_media_valida
    )
  ]
)

cat("\nMeses extremos entre meses representativos:\n")

print(
  extremos_mensais[
    ,
    .(
      no_estacao,
      ano,
      n_meses_representativos,
      nome_mes_menor_media,
      menor_media_mensal,
      nome_mes_maior_media,
      maior_media_mensal,
      amplitude_mensal
    )
  ]
)

cat("\nComparacao contemporanea CUPE x IPOJUCA 2021:\n")
print(comp_2021)

cat(
  "\nIMPORTANTE:\n",
  "- esta etapa e descritiva; nao atribui causalidade meteorologica;\n",
  "- meses nao representativos permanecem sinalizados;\n",
  "- autocorrelacoes usam defasagens de calendario e pares validos;\n",
  "- SUAPE 2022 participa da caracterizacao temporal do MP10 mesmo sem pareamento meteorologico;\n",
  "- a proxima etapa pode analisar associacoes MP10 x meteorologia para os cinco station-years integrados da Etapa 9O.\n",
  sep = ""
)
