# ============================================================
# ETAPA 9F - CONVENCAO DIARIA FINAL E ESTABILIDADE DOS PAREAMENTOS
# Projeto de Mestrado - MP10 / Suape
# ============================================================
#
# OBJETIVO
# Tornar principal a convencao temporal testada na Etapa 9E:
# registros CEMADEN exatamente a 00:00 local sao atribuidos ao
# dia civil anterior, pois o timestamp representa o fim do
# intervalo de acumulacao. Em seguida, recalcular as series diarias
# e testar se as conclusoes de pareamento permanecem estaveis.
#
# IMPORTANTE
# - nao imputa precipitacao;
# - nao exclui extremos positivos por magnitude;
# - utiliza apenas registros previamente validos na Etapa 9C;
# - preserva os criterios de continuidade da Etapa 9D;
# - nao escolhe automaticamente a estacao final;
# - registra explicitamente a limitacao de borda para 2025,
#   pois a base consolidada termina em 2025.
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

ARQ_LISTA <- file.path(
  "outputs", "09_meteorologia", "precipitacao",
  "completude_por_fonte",
  "lista_curta_candidatos_ate20km.csv"
)

ARQ_MP10 <- file.path(
  "outputs", "04_completude",
  "completude_diaria_mp10.csv"
)

ARQ_09E <- file.path(
  "outputs", "09_meteorologia", "precipitacao",
  "series_diarias_candidatos",
  "resumo_series_diarias_candidatos.csv"
)

SAIDA <- file.path(
  "outputs", "09_meteorologia", "precipitacao",
  "convencao_diaria_final"
)

dir.create(SAIDA, recursive = TRUE, showWarnings = FALSE)

for (f in c(
  ARQ_QAQC,
  ARQ_COMP_DIA,
  ARQ_LISTA,
  ARQ_MP10,
  ARQ_09E
)) {
  if (!file.exists(f)) {
    stop("Arquivo necessario nao encontrado: ", f)
  }
}

# ------------------------------------------------------------
# 2. FUNCOES
# ------------------------------------------------------------

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
  uniao <- sum(a | b)
  if (uniao == 0) return(NA_real_)
  sum(a & b) / uniao
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

  chuva0_a <- x[ok] > 0
  chuva0_b <- y[ok] > 0
  chuva1_a <- x[ok] >= 1
  chuva1_b <- y[ok] >= 1

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
      length(chuva0_a)
    ) {
      mean(chuva0_a == chuva0_b)
    } else {
      NA_real_
    },

    jaccard_chuva_maior_0 = jaccard_binario(
      chuva0_a,
      chuva0_b
    ),

    acordo_chuva_maior_igual_1mm = if (
      length(chuva1_a)
    ) {
      mean(chuva1_a == chuva1_b)
    } else {
      NA_real_
    },

    jaccard_chuva_maior_igual_1mm = jaccard_binario(
      chuva1_a,
      chuva1_b
    )
  )
}

# ------------------------------------------------------------
# 3. DOCUMENTAR CONVENCAO
# ------------------------------------------------------------

regras <- data.table(
  item = c(
    "fuso_operacional",
    "timestamp_intervalo",
    "registro_00h",
    "extremos",
    "conflitos",
    "completude",
    "borda_2025"
  ),
  regra = c(
    "America/Recife, derivado da conversao UTC realizada na Etapa 9A.",
    "O timestamp CEMADEN e interpretado como final do intervalo de acumulacao.",
    "Registro exatamente a 00:00 local e atribuido ao dia civil anterior.",
    "Valores positivos extremos permanecem preservados; nao ha exclusao por magnitude.",
    "Registros marcados como conflito na 9C nao entram nos totais finais.",
    "Mantem os criterios de continuidade da 9D: 60 min principal; 70 e 90 min como sensibilidades.",
    "Para 2025, a base nao contem 2026; portanto o total de 31/12/2025 pode nao incorporar eventual transmissao de 00:00 de 01/01/2026."
  )
)

fwrite(
  regras,
  file.path(
    SAIDA,
    "regras_convencao_diaria_final.csv"
  ),
  bom = TRUE
)

# ------------------------------------------------------------
# 4. CANDIDATOS
# ------------------------------------------------------------

cat("\nCarregando candidatos...\n")

lista <- fread(
  ARQ_LISTA,
  encoding = "UTF-8"
)

lista[
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
  lista[
    fonte_dados == "CEMADEN" &
      elegivel_pareamento_primario == TRUE,
    .(
      estacao_historica,
      ano_alvo,
      cod_estacao,
      nome_estacao,
      municipios,
      distancia_km
    )
  ]
)

if (!nrow(cand)) {
  stop("Nenhum candidato CEMADEN elegivel encontrado.")
}

codigos <- unique(cand$cod_estacao)
anos_alvo <- sort(unique(cand$ano_alvo))

# ------------------------------------------------------------
# 5. RECONSTRUIR PRECIPITACAO DIARIA COM 00:00 NO DIA ANTERIOR
# ------------------------------------------------------------

cat("Reconstruindo precipitacao diaria com a convencao de 00:00...\n")

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
    cod_estacao %in% codigos
]

if (!nrow(p)) {
  stop("Nenhum registro CEMADEN valido encontrado para os candidatos.")
}

# Mantem apenas o intervalo necessario: anos-alvo e o inicio
# do ano seguinte quando disponivel, para capturar 00:00 do dia 01/01.
ano_min <- min(anos_alvo)
ano_max <- min(max(anos_alvo) + 1L, 2025L)

p <- p[
  ano >= ano_min &
    ano <= ano_max
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

chuva_final <- p[
  ano_referencia %in% anos_alvo,
  .(
    precip_diaria_final_mm = sum(
      precipitacao_mm,
      na.rm = TRUE
    ),
    n_registros_final = .N,
    n_registros_00h_relocados = sum(
      hora_local_txt == "00:00:00"
    ),
    max_acumulado_intervalo_mm = max(
      precipitacao_mm,
      na.rm = TRUE
    )
  ),
  by = .(
    cod_estacao,
    ano_alvo = ano_referencia,
    data_local = data_referencia
  )
]

rm(p)
gc()

# ------------------------------------------------------------
# 6. COMPLETUDE DA 9D
# ------------------------------------------------------------

cat("Integrando completude da Etapa 9D...\n")

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
    cod_estacao %in% codigos &
    ano %in% anos_alvo,
  .(
    cod_estacao,
    ano_alvo = ano,
    data_local,
    n_obs_validas,
    maior_gap_dia_min,
    cobertura_60 = cobertura_primaria,
    cobertura_70 = cobertura_sens_1,
    cobertura_90 = cobertura_sens_2
  )
]

# ------------------------------------------------------------
# 7. MP10 DIARIO
# ------------------------------------------------------------

mp10 <- fread(
  ARQ_MP10,
  encoding = "UTF-8"
)

mp10[
  ,
  `:=`(
    estacao_historica = as.character(
      no_estacao
    ),
    ano_alvo = as.integer(
      ano
    ),
    data_local = as.IDate(
      data
    )
  )
]

mp10 <- mp10[
  ,
  .(
    estacao_historica,
    ano_alvo,
    data_local,
    dia_mp10_valido = dia_valido_mma_16h,
    media_mp10 = media_24h_mma
  )
]

# ------------------------------------------------------------
# 8. GRADE DIARIA FINAL
# ------------------------------------------------------------

cat("Construindo grade diaria final...\n")

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
    distancia_km
  )
]

serie <- merge(
  grade,
  chuva_final,
  by = c(
    "cod_estacao",
    "ano_alvo",
    "data_local"
  ),
  all.x = TRUE
)

serie <- merge(
  serie,
  comp,
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
  tem_precipitacao_final := !is.na(
    precip_diaria_final_mm
  )
]

serie[
  ,
  flag_borda_2025 := (
    ano_alvo == 2025L &
      format(
        data_local,
        "%m-%d"
      ) == "12-31"
  )
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
    "series_diarias_precipitacao_convencao_final.csv"
  ),
  bom = TRUE
)

# ------------------------------------------------------------
# 9. RESUMO DOS CANDIDATOS
# ------------------------------------------------------------

resumo_final <- serie[
  ,
  .(
    distancia_km = unique(
      distancia_km
    )[1],

    n_dias_ano = .N,

    n_dias_com_precipitacao = sum(
      tem_precipitacao_final
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
      precip_diaria_final_mm[
        cobertura_60 %in% TRUE
      ],
      na.rm = TRUE
    ),

    total_mm_dias_70 = sum(
      precip_diaria_final_mm[
        cobertura_70 %in% TRUE
      ],
      na.rm = TRUE
    ),

    total_mm_dias_90 = sum(
      precip_diaria_final_mm[
        cobertura_90 %in% TRUE
      ],
      na.rm = TRUE
    ),

    max_precip_diaria_final_mm = if (
      all(
        is.na(
          precip_diaria_final_mm
        )
      )
    ) {
      NA_real_
    } else {
      max(
        precip_diaria_final_mm,
        na.rm = TRUE
      )
    },

    n_registros_00h_relocados = sum(
      n_registros_00h_relocados,
      na.rm = TRUE
    )
  ),
  by = .(
    estacao_historica,
    ano_alvo,
    cod_estacao,
    nome_estacao,
    municipios
  )
]

resumo_final[
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
  resumo_final,
  estacao_historica,
  ano_alvo,
  distancia_km
)

fwrite(
  resumo_final,
  file.path(
    SAIDA,
    "resumo_candidatos_convencao_final.csv"
  ),
  bom = TRUE
)

# ------------------------------------------------------------
# 10. COMPARAR COM A ETAPA 9E
# ------------------------------------------------------------

antigo <- fread(
  ARQ_09E,
  encoding = "UTF-8"
)

antigo[
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

comparacao_09e_09f <- merge(
  antigo[
    ,
    .(
      estacao_historica,
      ano_alvo,
      cod_estacao,
      nome_estacao,
      max_precip_diaria_09e =
        max_precip_diaria_mm
    )
  ],
  resumo_final[
    ,
    .(
      estacao_historica,
      ano_alvo,
      cod_estacao,
      max_precip_diaria_09f =
        max_precip_diaria_final_mm,
      n_registros_00h_relocados
    )
  ],
  by = c(
    "estacao_historica",
    "ano_alvo",
    "cod_estacao"
  ),
  all = TRUE
)

comparacao_09e_09f[
  ,
  diferenca_max_diaria_mm :=
    max_precip_diaria_09f -
    max_precip_diaria_09e
]

fwrite(
  comparacao_09e_09f,
  file.path(
    SAIDA,
    "comparacao_resumos_09e_09f.csv"
  ),
  bom = TRUE
)

# ------------------------------------------------------------
# 11. COMPARACOES PAREADAS COM CONVENCAO FINAL
# ------------------------------------------------------------

cat("Recalculando comparacoes pareadas...\n")

alvos <- unique(
  cand[
    ,
    .(
      estacao_historica,
      ano_alvo
    )
  ]
)

resultados <- list()
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
      estacao_historica == alvo_est &
        ano_alvo == alvo_ano &
        cod_estacao == ca$cod_estacao,
      .(
        data_local,
        precip_a = precip_diaria_final_mm,
        cobertura_60_a = cobertura_60,
        cobertura_70_a = cobertura_70,
        cobertura_90_a = cobertura_90,
        dia_mp10_valido
      )
    ]

    b <- serie[
      estacao_historica == alvo_est &
        ano_alvo == alvo_ano &
        cod_estacao == cb$cod_estacao,
      .(
        data_local,
        precip_b = precip_diaria_final_mm,
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
    "comparacao_pareada_convencao_final.csv"
  ),
  bom = TRUE
)

# ------------------------------------------------------------
# 12. NUCLEO LOCAL E PAREAMENTOS PROVISORIOS
# ------------------------------------------------------------

codigos_nucleo <- c(
  "260720801A", # Centro
  "260720804A", # Ruropolis
  "260720801G"  # Campo do Aviao
)

nucleo <- pares[
  cod_estacao_a %in% codigos_nucleo &
    cod_estacao_b %in% codigos_nucleo
]

fwrite(
  nucleo,
  file.path(
    SAIDA,
    "comparacao_nucleo_ipojuca_convencao_final.csv"
  ),
  bom = TRUE
)

# Tabela de acompanhamento dos pareamentos que vinham sendo considerados.
pareamentos_provisorios <- data.table(
  estacao_historica = c(
    "IFPE",
    "CUPE",
    "IPOJUCA",
    "IPOJUCA"
  ),
  ano_alvo = c(
    2018L,
    2021L,
    2021L,
    2025L
  ),
  principal_provisorio = c(
    "260720801A",
    "260720801A",
    "260720801A",
    "260720804A"
  ),
  sensibilidade_provisoria = c(
    "260720804A",
    "260720804A",
    "260720804A",
    "260720801G"
  ),
  status_antes_09f = c(
    "proposto",
    "provisorio_por_proxy_espacial_CUPE",
    "proposto",
    "proposto"
  )
)

avaliacao_provisoria <- merge(
  pareamentos_provisorios,
  resumo_final[
    ,
    .(
      estacao_historica,
      ano_alvo,
      cod_estacao = cod_estacao,
      nome_estacao,
      distancia_km,
      pct_overlap_60_mp10,
      pct_overlap_70_mp10,
      pct_overlap_90_mp10
    )
  ],
  by.x = c(
    "estacao_historica",
    "ano_alvo",
    "principal_provisorio"
  ),
  by.y = c(
    "estacao_historica",
    "ano_alvo",
    "cod_estacao"
  ),
  all.x = TRUE
)

setnames(
  avaliacao_provisoria,
  old = c(
    "nome_estacao",
    "distancia_km",
    "pct_overlap_60_mp10",
    "pct_overlap_70_mp10",
    "pct_overlap_90_mp10"
  ),
  new = c(
    "nome_principal",
    "distancia_principal_km",
    "overlap60_principal",
    "overlap70_principal",
    "overlap90_principal"
  )
)

tmp_sens <- resumo_final[
  ,
  .(
    estacao_historica,
    ano_alvo,
    sensibilidade_provisoria = cod_estacao,
    nome_sensibilidade = nome_estacao,
    distancia_sensibilidade_km = distancia_km,
    overlap60_sensibilidade =
      pct_overlap_60_mp10,
    overlap70_sensibilidade =
      pct_overlap_70_mp10,
    overlap90_sensibilidade =
      pct_overlap_90_mp10
  )
]

avaliacao_provisoria <- merge(
  avaliacao_provisoria,
  tmp_sens,
  by = c(
    "estacao_historica",
    "ano_alvo",
    "sensibilidade_provisoria"
  ),
  all.x = TRUE
)

fwrite(
  avaliacao_provisoria,
  file.path(
    SAIDA,
    "avaliacao_pareamentos_provisorios.csv"
  ),
  bom = TRUE
)

# ------------------------------------------------------------
# 13. RESUMO
# ------------------------------------------------------------

resumo_execucao <- data.table(
  indicador = c(
    "n_alvos_mp10",
    "n_combinacoes_candidato_alvo",
    "n_codigos_cemaden_distintos",
    "n_linhas_series_diarias",
    "n_comparacoes_pareadas_criterio",
    "n_comparacoes_nucleo",
    "n_registros_00h_relocados_total",
    "n_linhas_borda_31dez2025"
  ),
  valor = c(
    nrow(alvos),
    nrow(cand),
    uniqueN(cand$cod_estacao),
    nrow(serie),
    nrow(pares),
    nrow(nucleo),
    sum(
      resumo_final$n_registros_00h_relocados,
      na.rm = TRUE
    ),
    sum(
      serie$flag_borda_2025,
      na.rm = TRUE
    )
  )
)

fwrite(
  resumo_execucao,
  file.path(
    SAIDA,
    "resumo_execucao_convencao_final.csv"
  ),
  bom = TRUE
)

cat("\n============================================================\n")
cat("ETAPA 9F - CONVENCAO DIARIA FINAL CONCLUIDA\n")
cat("============================================================\n\n")

print(resumo_execucao)

cat("\nPareamentos provisorios para revisao apos a nova convencao:\n")
print(avaliacao_provisoria)

cat(
  "\nIMPORTANTE:\n",
  "- 00:00 local agora pertence ao dia civil anterior;\n",
  "- o criterio de completude permanece o da Etapa 9D;\n",
  "- 31/12/2025 permanece com limitacao de borda por ausencia de 2026;\n",
  "- esta etapa testa estabilidade; nao substitui a validacao espacial de Gaibu e Suape.\n",
  sep = ""
)
