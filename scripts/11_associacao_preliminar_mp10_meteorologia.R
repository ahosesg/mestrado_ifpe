# ============================================================
# ETAPA 11 - ASSOCIACAO PRELIMINAR ENTRE MP10 E METEOROLOGIA
# Projeto de Mestrado - MP10 / Suape
# ============================================================
#
# OBJETIVO
# Avaliar, de forma exploratoria e reprodutivel, a associacao entre
# as concentracoes diarias validas de MP10 e as principais variaveis
# meteorologicas nos cinco station-years com pareamento fechado na
# Etapa 9.
#
# PRINCIPIOS
# - analise separada por station-year;
# - uso de pares validos especificos por variavel;
# - Spearman como medida principal de associacao monotona;
# - Pearson como medida complementar;
# - nenhuma correlacao direta com graus de direcao do vento;
# - direcao tratada por componentes u/v e setores;
# - precipitacao avaliada no mesmo dia e com lags de 1 e 2 dias;
# - analise adicional com anomalias intramensais em meses
#   representativos de MP10, reduzindo o efeito de sazonalidade;
# - nao calcular p-valores inferenciais nesta etapa, pois a
#   autocorrelacao temporal viola a hipotese de independencia simples;
# - resultados sao associativos, nao causais.
# ============================================================

if (!requireNamespace("data.table", quietly = TRUE)) {
  stop("Instale o pacote 'data.table' antes de executar.")
}
library(data.table)

# ------------------------------------------------------------
# 1. PARAMETROS E CAMINHOS
# ------------------------------------------------------------

MIN_PARES <- 30L

ARQ_BASE <- file.path(
  "outputs", "09_meteorologia",
  "consolidacao_final",
  "base_diaria_principal_mp10_meteorologia.csv"
)

ARQ_MENSAL <- file.path(
  "outputs", "10_caracterizacao_temporal",
  "estatisticas_mensais_mp10.csv"
)

SAIDA <- file.path(
  "outputs", "11_associacao_meteorologia"
)

dir.create(
  SAIDA,
  recursive = TRUE,
  showWarnings = FALSE
)

for (f in c(
  ARQ_BASE,
  ARQ_MENSAL
)) {
  if (!file.exists(f)) {
    stop(
      "Arquivo necessario nao encontrado: ",
      f
    )
  }
}

# ------------------------------------------------------------
# 2. FUNCOES
# ------------------------------------------------------------

cor_segura <- function(
  x,
  y,
  metodo = "spearman",
  min_pares = MIN_PARES
) {
  ok <- is.finite(x) & is.finite(y)

  if (sum(ok) < min_pares) {
    return(NA_real_)
  }

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

media_segura <- function(x) {
  z <- x[is.finite(x)]
  if (!length(z)) return(NA_real_)
  mean(z)
}

mediana_segura <- function(x) {
  z <- x[is.finite(x)]
  if (!length(z)) return(NA_real_)
  median(z)
}

sd_seguro <- function(x) {
  z <- x[is.finite(x)]
  if (length(z) < 2L) return(NA_real_)
  sd(z)
}

iqr_seguro <- function(x) {
  z <- x[is.finite(x)]
  if (!length(z)) return(NA_real_)
  IQR(z)
}

anomalia_mediana <- function(x) {
  med <- mediana_segura(x)
  if (!is.finite(med)) {
    return(rep(NA_real_, length(x)))
  }
  x - med
}

setor_vento_8 <- function(graus) {

  x <- graus %% 360

  out <- rep(
    NA_character_,
    length(x)
  )

  out[
    is.finite(x) &
      (x >= 337.5 | x < 22.5)
  ] <- "N"

  out[
    is.finite(x) &
      x >= 22.5 &
      x < 67.5
  ] <- "NE"

  out[
    is.finite(x) &
      x >= 67.5 &
      x < 112.5
  ] <- "E"

  out[
    is.finite(x) &
      x >= 112.5 &
      x < 157.5
  ] <- "SE"

  out[
    is.finite(x) &
      x >= 157.5 &
      x < 202.5
  ] <- "S"

  out[
    is.finite(x) &
      x >= 202.5 &
      x < 247.5
  ] <- "SW"

  out[
    is.finite(x) &
      x >= 247.5 &
      x < 292.5
  ] <- "W"

  out[
    is.finite(x) &
      x >= 292.5 &
      x < 337.5
  ] <- "NW"

  factor(
    out,
    levels = c(
      "N", "NE", "E", "SE",
      "S", "SW", "W", "NW"
    ),
    ordered = TRUE
  )
}

# ------------------------------------------------------------
# 3. CARREGAR BASE FINAL E NORMALIZAR
# ------------------------------------------------------------

cat("\nCarregando base integrada MP10 + meteorologia...\n")

base <- fread(
  ARQ_BASE,
  encoding = "UTF-8"
)

mensal <- fread(
  ARQ_MENSAL,
  encoding = "UTF-8"
)

necessarias <- c(
  "estacao_historica",
  "ano_alvo",
  "data_local",
  "dia_mp10_valido",
  "mp10_media_24h",
  "temperatura_media_c",
  "umidade_media_pct",
  "velocidade_vento_media_ms",
  "pressao_media_hpa",
  "vento_u_medio_ms",
  "vento_v_medio_ms",
  "direcao_vetor_medio_graus",
  "precip_principal_mm",
  "precip_principal_valida_60"
)

faltantes <- setdiff(
  necessarias,
  names(base)
)

if (length(faltantes)) {
  stop(
    "Colunas ausentes na base final da Etapa 9O: ",
    paste(
      faltantes,
      collapse = ", "
    )
  )
}

base[
  ,
  `:=`(
    estacao_historica =
      as.character(estacao_historica),
    ano_alvo =
      as.integer(ano_alvo),
    data_local =
      as.IDate(data_local),
    mes =
      as.integer(
        format(
          as.IDate(data_local),
          "%m"
        )
      )
  )
]

mensal[
  ,
  `:=`(
    no_estacao =
      as.character(no_estacao),
    ano =
      as.integer(ano),
    mes =
      as.integer(mes)
  )
]

# Flag de representatividade mensal do MP10.
mensal_flag <- mensal[
  ,
  .(
    estacao_historica =
      no_estacao,
    ano_alvo =
      ano,
    mes,
    mes_representativo_mma
  )
]

base <- merge(
  base,
  mensal_flag,
  by = c(
    "estacao_historica",
    "ano_alvo",
    "mes"
  ),
  all.x = TRUE
)

if (
  any(
    is.na(
      base$mes_representativo_mma
    )
  )
) {
  stop(
    "Nao foi possivel recuperar a representatividade mensal para todas as linhas."
  )
}

setorder(
  base,
  estacao_historica,
  ano_alvo,
  data_local
)

# ------------------------------------------------------------
# 4. GARANTIR GRADE CALENDARIO E CRIAR LAGS DE PRECIPITACAO
# ------------------------------------------------------------

cat("Criando lags de precipitacao em dias de calendario...\n")

aud_grade <- base[
  ,
  .(
    n_dias = .N,
    n_datas_unicas =
      uniqueN(data_local),
    primeira_data =
      min(data_local),
    ultima_data =
      max(data_local)
  ),
  by = .(
    estacao_historica,
    ano_alvo
  )
]

aud_grade[
  ,
  grade_ok := (
    n_dias == 365L &
      n_datas_unicas == 365L
  )
]

fwrite(
  aud_grade,
  file.path(
    SAIDA,
    "auditoria_grade_base_associacao.csv"
  ),
  bom = TRUE
)

if (any(!aud_grade$grade_ok)) {
  stop(
    "A base integrada nao possui grade diaria completa em um ou mais station-years."
  )
}

base[
  ,
  precip_lag1_mm := shift(
    precip_principal_mm,
    n = 1L,
    type = "lag"
  ),
  by = .(
    estacao_historica,
    ano_alvo
  )
]

base[
  ,
  precip_lag2_mm := shift(
    precip_principal_mm,
    n = 2L,
    type = "lag"
  ),
  by = .(
    estacao_historica,
    ano_alvo
  )
]

base[
  ,
  precip_valida_lag1 := shift(
    precip_principal_valida_60,
    n = 1L,
    type = "lag"
  ),
  by = .(
    estacao_historica,
    ano_alvo
  )
]

base[
  ,
  precip_valida_lag2 := shift(
    precip_principal_valida_60,
    n = 2L,
    type = "lag"
  ),
  by = .(
    estacao_historica,
    ano_alvo
  )
]

# ------------------------------------------------------------
# 5. BASE ANALITICA E VARIAVEIS
# ------------------------------------------------------------

base[
  ,
  mp10 := fifelse(
    dia_mp10_valido %in% TRUE,
    as.numeric(
      mp10_media_24h
    ),
    NA_real_
  )
]

base[
  ,
  precip0 := fifelse(
    precip_principal_valida_60 %in% TRUE,
    as.numeric(
      precip_principal_mm
    ),
    NA_real_
  )
]

base[
  ,
  precip1 := fifelse(
    precip_valida_lag1 %in% TRUE,
    as.numeric(
      precip_lag1_mm
    ),
    NA_real_
  )
]

base[
  ,
  precip2 := fifelse(
    precip_valida_lag2 %in% TRUE,
    as.numeric(
      precip_lag2_mm
    ),
    NA_real_
  )
]

variaveis <- data.table(
  variavel = c(
    "temperatura_media_c",
    "umidade_media_pct",
    "velocidade_vento_media_ms",
    "pressao_media_hpa",
    "vento_u_medio_ms",
    "vento_v_medio_ms",
    "precipitacao_lag0_mm",
    "precipitacao_lag1_mm",
    "precipitacao_lag2_mm"
  ),
  coluna = c(
    "temperatura_media_c",
    "umidade_media_pct",
    "velocidade_vento_media_ms",
    "pressao_media_hpa",
    "vento_u_medio_ms",
    "vento_v_medio_ms",
    "precip0",
    "precip1",
    "precip2"
  ),
  grupo = c(
    "temperatura",
    "umidade",
    "vento",
    "pressao",
    "vento_componente",
    "vento_componente",
    "precipitacao",
    "precipitacao",
    "precipitacao"
  )
)

# ------------------------------------------------------------
# 6. CORRELACOES BRUTAS POR STATION-YEAR
# ------------------------------------------------------------

cat("Calculando correlacoes brutas por station-year...\n")

chaves <- unique(
  base[
    ,
    .(
      estacao_historica,
      ano_alvo
    )
  ]
)

lista_cor <- list()
k <- 0L

for (i in seq_len(nrow(chaves))) {

  est <- chaves$estacao_historica[i]
  ano <- chaves$ano_alvo[i]

  z <- base[
    estacao_historica == est &
      ano_alvo == ano
  ]

  for (j in seq_len(nrow(variaveis))) {

    var_nome <- variaveis$variavel[j]
    col_nome <- variaveis$coluna[j]
    grupo <- variaveis$grupo[j]

    x <- z$mp10
    y <- z[[col_nome]]

    ok <- is.finite(x) &
      is.finite(y)

    k <- k + 1L

    lista_cor[[k]] <- data.table(
      estacao_historica = est,
      ano_alvo = ano,
      variavel = var_nome,
      grupo = grupo,
      n_pares = sum(ok),
      pct_dias_mp10_com_par =
        if (
          sum(
            is.finite(x)
          ) > 0
        ) {
          100 *
            sum(ok) /
            sum(
              is.finite(x)
            )
        } else {
          NA_real_
        },
      spearman =
        cor_segura(
          x,
          y,
          "spearman"
        ),
      pearson =
        cor_segura(
          x,
          y,
          "pearson"
        )
    )
  }
}

cor_bruta <- rbindlist(
  lista_cor,
  fill = TRUE
)

fwrite(
  cor_bruta,
  file.path(
    SAIDA,
    "correlacoes_brutas_mp10_meteorologia.csv"
  ),
  bom = TRUE
)

# ------------------------------------------------------------
# 7. ANOMALIAS INTRAMENSAIS EM MESES REPRESENTATIVOS
# ------------------------------------------------------------

cat("Calculando associacoes com anomalias intramensais...\n")

# Anomalias sao centradas pela mediana de cada mes, dentro de cada
# station-year. Isso reduz o efeito da sazonalidade ampla sem impor
# modelo paramétrico.
base[
  mes_representativo_mma %in% TRUE,
  mp10_anom_mes :=
    anomalia_mediana(mp10),
  by = .(
    estacao_historica,
    ano_alvo,
    mes
  )
]

for (col_nome in variaveis$coluna) {

  nome_anom <- paste0(
    col_nome,
    "_anom_mes"
  )

  base[
    mes_representativo_mma %in% TRUE,
    (nome_anom) :=
      anomalia_mediana(
        get(col_nome)
      ),
    by = .(
      estacao_historica,
      ano_alvo,
      mes
    )
  ]
}

lista_anom <- list()
k2 <- 0L

for (i in seq_len(nrow(chaves))) {

  est <- chaves$estacao_historica[i]
  ano <- chaves$ano_alvo[i]

  z <- base[
    estacao_historica == est &
      ano_alvo == ano &
      mes_representativo_mma %in% TRUE
  ]

  for (j in seq_len(nrow(variaveis))) {

    var_nome <- variaveis$variavel[j]
    col_nome <- variaveis$coluna[j]
    grupo <- variaveis$grupo[j]

    y_col <- paste0(
      col_nome,
      "_anom_mes"
    )

    x <- z$mp10_anom_mes
    y <- z[[y_col]]

    ok <- is.finite(x) &
      is.finite(y)

    k2 <- k2 + 1L

    lista_anom[[k2]] <- data.table(
      estacao_historica = est,
      ano_alvo = ano,
      variavel = var_nome,
      grupo = grupo,
      n_pares = sum(ok),
      spearman_anomalia_mensal =
        cor_segura(
          x,
          y,
          "spearman"
        ),
      pearson_anomalia_mensal =
        cor_segura(
          x,
          y,
          "pearson"
        )
    )
  }
}

cor_anom <- rbindlist(
  lista_anom,
  fill = TRUE
)

fwrite(
  cor_anom,
  file.path(
    SAIDA,
    "correlacoes_anomalias_intramensais.csv"
  ),
  bom = TRUE
)

# ------------------------------------------------------------
# 8. COMPARAR CORRELACAO BRUTA E AJUSTADA POR SAZONALIDADE
# ------------------------------------------------------------

comparacao_cor <- merge(
  cor_bruta,
  cor_anom[
    ,
    .(
      estacao_historica,
      ano_alvo,
      variavel,
      n_pares_anomalia =
        n_pares,
      spearman_anomalia_mensal,
      pearson_anomalia_mensal
    )
  ],
  by = c(
    "estacao_historica",
    "ano_alvo",
    "variavel"
  ),
  all.x = TRUE
)

comparacao_cor[
  ,
  `:=`(
    diferenca_spearman =
      spearman_anomalia_mensal -
        spearman,
    sinal_bruto =
      fifelse(
        is.na(spearman),
        NA_character_,
        fifelse(
          spearman > 0,
          "positivo",
          fifelse(
            spearman < 0,
            "negativo",
            "zero"
          )
        )
      ),
    sinal_anomalia =
      fifelse(
        is.na(
          spearman_anomalia_mensal
        ),
        NA_character_,
        fifelse(
          spearman_anomalia_mensal > 0,
          "positivo",
          fifelse(
            spearman_anomalia_mensal < 0,
            "negativo",
            "zero"
          )
        )
      )
  )
]

comparacao_cor[
  ,
  sinal_mantido := (
    !is.na(sinal_bruto) &
      !is.na(sinal_anomalia) &
      sinal_bruto ==
        sinal_anomalia
  )
]

fwrite(
  comparacao_cor,
  file.path(
    SAIDA,
    "comparacao_correlacao_bruta_vs_intramensal.csv"
  ),
  bom = TRUE
)

# ------------------------------------------------------------
# 9. CHUVA: DIAS SECOS X CHUVOSOS
# ------------------------------------------------------------

cat("Comparando MP10 em dias secos e chuvosos...\n")

chuva_dias <- base[
  is.finite(mp10) &
    is.finite(precip0)
]

chuva_dias[
  ,
  classe_chuva := fifelse(
    precip0 > 0,
    "chuvoso",
    "seco"
  )
]

resumo_chuva <- chuva_dias[
  ,
  .(
    n_dias = .N,
    mp10_media =
      media_segura(mp10),
    mp10_mediana =
      mediana_segura(mp10),
    mp10_sd =
      sd_seguro(mp10),
    mp10_iqr =
      iqr_seguro(mp10),
    precip_media_mm =
      media_segura(precip0)
  ),
  by = .(
    estacao_historica,
    ano_alvo,
    classe_chuva
  )
]

seco <- resumo_chuva[
  classe_chuva == "seco",
  .(
    estacao_historica,
    ano_alvo,
    n_secos =
      n_dias,
    mp10_media_seco =
      mp10_media,
    mp10_mediana_seco =
      mp10_mediana
  )
]

chuvoso <- resumo_chuva[
  classe_chuva == "chuvoso",
  .(
    estacao_historica,
    ano_alvo,
    n_chuvosos =
      n_dias,
    mp10_media_chuvoso =
      mp10_media,
    mp10_mediana_chuvoso =
      mp10_mediana
  )
]

contraste_chuva <- merge(
  seco,
  chuvoso,
  by = c(
    "estacao_historica",
    "ano_alvo"
  ),
  all = TRUE
)

contraste_chuva[
  ,
  `:=`(
    diferenca_media_chuvoso_menos_seco =
      mp10_media_chuvoso -
        mp10_media_seco,
    diferenca_mediana_chuvoso_menos_seco =
      mp10_mediana_chuvoso -
        mp10_mediana_seco,
    razao_medias_chuvoso_seco =
      fifelse(
        is.finite(
          mp10_media_seco
        ) &
          mp10_media_seco != 0,
        mp10_media_chuvoso /
          mp10_media_seco,
        NA_real_
      )
  )
]

fwrite(
  resumo_chuva,
  file.path(
    SAIDA,
    "mp10_dias_secos_chuvosos.csv"
  ),
  bom = TRUE
)

fwrite(
  contraste_chuva,
  file.path(
    SAIDA,
    "contraste_mp10_dias_chuvosos_secos.csv"
  ),
  bom = TRUE
)

# ------------------------------------------------------------
# 10. DIRECAO DO VENTO POR SETORES
# ------------------------------------------------------------

cat("Resumindo MP10 por setor de vento...\n")

base[
  ,
  setor_vento :=
    setor_vento_8(
      direcao_vetor_medio_graus
    )
]

setores <- base[
  is.finite(mp10) &
    !is.na(setor_vento),
  .(
    n_dias = .N,
    mp10_media =
      media_segura(mp10),
    mp10_mediana =
      mediana_segura(mp10),
    mp10_sd =
      sd_seguro(mp10),
    velocidade_vento_media =
      media_segura(
        velocidade_vento_media_ms
      ),
    percentual_dias_validos_mp10 =
      100 * .N /
        sum(
          is.finite(
            base[
              estacao_historica ==
                .BY$estacao_historica &
                ano_alvo ==
                  .BY$ano_alvo,
              mp10
            ]
          )
        )
  ),
  by = .(
    estacao_historica,
    ano_alvo,
    setor_vento
  )
]

setores[
  ,
  setor_com_n_adequado :=
    n_dias >= 10L
]

fwrite(
  setores,
  file.path(
    SAIDA,
    "mp10_por_setor_de_vento.csv"
  ),
  bom = TRUE
)

# ------------------------------------------------------------
# 11. COBERTURA ANALITICA FINAL
# ------------------------------------------------------------

cobertura <- cor_bruta[
  ,
  .(
    n_pares,
    pct_dias_mp10_com_par
  ),
  by = .(
    estacao_historica,
    ano_alvo,
    variavel
  )
]

fwrite(
  cobertura,
  file.path(
    SAIDA,
    "cobertura_analitica_por_variavel.csv"
  ),
  bom = TRUE
)

# ------------------------------------------------------------
# 12. SINTESE DESCRITIVA DOS SINAIS
# ------------------------------------------------------------

# Sintese apenas descritiva. Nao representa teste de consistencia
# estatistica entre station-years.
sintese_sinais <- comparacao_cor[
  is.finite(spearman),
  .(
    n_station_years =
      .N,
    n_spearman_positivo =
      sum(
        spearman > 0
      ),
    n_spearman_negativo =
      sum(
        spearman < 0
      ),
    mediana_spearman =
      mediana_segura(
        spearman
      ),
    n_sinal_mantido_anomalia =
      sum(
        sinal_mantido %in% TRUE,
        na.rm = TRUE
      )
  ),
  by = .(
    variavel
  )
]

fwrite(
  sintese_sinais,
  file.path(
    SAIDA,
    "sintese_descritiva_sinais_associacao.csv"
  ),
  bom = TRUE
)

# ------------------------------------------------------------
# 13. GRAFICOS PRELIMINARES
# ------------------------------------------------------------

cat("Gerando graficos preliminares...\n")

# 13.1 Painel de Spearman bruto.
vars_graf <- c(
  "temperatura_media_c",
  "umidade_media_pct",
  "velocidade_vento_media_ms",
  "pressao_media_hpa",
  "precipitacao_lag0_mm"
)

cor_plot <- cor_bruta[
  variavel %in% vars_graf
]

ord_series <- chaves[
  order(
    ano_alvo,
    estacao_historica
  )
]

mat_bruta <- matrix(
  NA_real_,
  nrow = nrow(ord_series),
  ncol = length(vars_graf)
)

rownames(mat_bruta) <- paste(
  ord_series$estacao_historica,
  ord_series$ano_alvo
)

colnames(mat_bruta) <- c(
  "Temp",
  "UR",
  "Vento",
  "Pressao",
  "Precip"
)

for (i in seq_len(nrow(ord_series))) {
  for (j in seq_along(vars_graf)) {

    z <- cor_plot[
      estacao_historica ==
        ord_series$estacao_historica[i] &
        ano_alvo ==
          ord_series$ano_alvo[i] &
        variavel ==
          vars_graf[j]
    ]

    if (nrow(z)) {
      mat_bruta[i, j] <-
        z$spearman[1]
    }
  }
}

png(
  file.path(
    SAIDA,
    "heatmap_spearman_bruto.png"
  ),
  width = 1500,
  height = 1000,
  res = 180
)

par(
  mar = c(7, 9, 4, 2)
)

image(
  x = seq_len(
    ncol(mat_bruta)
  ),
  y = seq_len(
    nrow(mat_bruta)
  ),
  z = t(mat_bruta[
    nrow(mat_bruta):1,
    ,
    drop = FALSE
  ]),
  zlim = c(-1, 1),
  xaxt = "n",
  yaxt = "n",
  xlab = "",
  ylab = "",
  main = "Spearman bruto: MP10 x meteorologia"
)

axis(
  1,
  at = seq_len(
    ncol(mat_bruta)
  ),
  labels = colnames(mat_bruta),
  las = 2
)

axis(
  2,
  at = seq_len(
    nrow(mat_bruta)
  ),
  labels = rev(
    rownames(mat_bruta)
  ),
  las = 2
)

for (i in seq_len(nrow(mat_bruta))) {
  for (j in seq_len(ncol(mat_bruta))) {

    val <- mat_bruta[i, j]

    if (is.finite(val)) {
      text(
        j,
        nrow(mat_bruta) - i + 1,
        labels = sprintf(
          "%.2f",
          val
        )
      )
    }
  }
}

dev.off()

# 13.2 Painel chuva seco/chuvoso.
png(
  file.path(
    SAIDA,
    "painel_mp10_seco_chuvoso.png"
  ),
  width = 1800,
  height = 1100,
  res = 180
)

par(
  mfrow = c(2, 3),
  mar = c(4, 4, 3, 1)
)

for (i in seq_len(nrow(chaves))) {

  est <- chaves$estacao_historica[i]
  ano <- chaves$ano_alvo[i]

  z <- chuva_dias[
    estacao_historica == est &
      ano_alvo == ano
  ]

  if (nrow(z)) {
    boxplot(
      mp10 ~ classe_chuva,
      data = z,
      xlab = "",
      ylab = expression(
        MP[10]~(mu*g/m^3)
      ),
      main = paste(
        est,
        ano
      ),
      names = c(
        "Chuvoso",
        "Seco"
      )
    )
  }
}

dev.off()

# ------------------------------------------------------------
# 14. DOCUMENTAR REGRAS
# ------------------------------------------------------------

regras <- data.table(
  item = c(
    "unidade_analise",
    "correlacao_principal",
    "correlacao_complementar",
    "minimo_pares",
    "sazonalidade",
    "meses_anomalia",
    "precipitacao",
    "direcao_vento",
    "p_valores",
    "causalidade",
    "complete_case_global"
  ),
  regra = c(
    "station-year; nao agregar automaticamente entre estacoes ou anos",
    "Spearman, com pares validos especificos por variavel",
    "Pearson, como sensibilidade a associacao linear",
    paste0(
      MIN_PARES,
      " pares validos para reportar correlacao"
    ),
    "comparar associacao bruta com anomalias centradas pela mediana mensal",
    "anomalias calculadas apenas em meses representativos de MP10",
    "avaliar lag 0, 1 e 2 dias; seco versus chuvoso apenas quando a precipitacao e valida",
    "nao correlacionar graus diretamente; usar componentes u/v e setores de 45 graus",
    "nao calculados nesta etapa por autocorrelacao temporal e multiplicidade de comparacoes",
    "resultados interpretados como associacoes exploratorias, nao efeitos causais",
    "nao utilizar; cada variavel usa sua propria disponibilidade"
  )
)

fwrite(
  regras,
  file.path(
    SAIDA,
    "regras_analise_associacao_preliminar.csv"
  ),
  bom = TRUE
)

# ------------------------------------------------------------
# 15. RESUMO EXECUCAO
# ------------------------------------------------------------

resumo_exec <- data.table(
  indicador = c(
    "n_station_years_analisados",
    "n_variaveis_correlacao",
    "n_correlacoes_brutas",
    "n_correlacoes_intramensais",
    "n_station_years_chuva_seco",
    "n_setores_vento_resumidos",
    "minimo_pares_correlacao",
    "n_p_valores_calculados",
    "n_graficos_gerados"
  ),
  valor = c(
    nrow(chaves),
    nrow(variaveis),
    sum(
      is.finite(
        cor_bruta$spearman
      )
    ),
    sum(
      is.finite(
        cor_anom$spearman_anomalia_mensal
      )
    ),
    uniqueN(
      contraste_chuva[
        !is.na(
          n_secos
        ) &
          !is.na(
            n_chuvosos
          ),
        paste(
          estacao_historica,
          ano_alvo
        )
      ]
    ),
    nrow(setores),
    MIN_PARES,
    0L,
    2L
  )
)

fwrite(
  resumo_exec,
  file.path(
    SAIDA,
    "resumo_execucao_associacao_meteorologia.csv"
  ),
  bom = TRUE
)

# ------------------------------------------------------------
# 16. CONSOLE
# ------------------------------------------------------------

cat("\n============================================================\n")
cat("ETAPA 11 - ASSOCIACAO PRELIMINAR MP10 X METEOROLOGIA CONCLUIDA\n")
cat("============================================================\n\n")

print(resumo_exec)

cat("\nCorrelacoes de Spearman brutas, variaveis principais:\n")

print(
  cor_bruta[
    variavel %in%
      c(
        "temperatura_media_c",
        "umidade_media_pct",
        "velocidade_vento_media_ms",
        "pressao_media_hpa",
        "precipitacao_lag0_mm"
      ),
    .(
      estacao_historica,
      ano_alvo,
      variavel,
      n_pares,
      pct_dias_mp10_com_par,
      spearman,
      pearson
    )
  ]
)

cat("\nCorrelacoes de Spearman apos centragem intramensal:\n")

print(
  comparacao_cor[
    variavel %in%
      c(
        "temperatura_media_c",
        "umidade_media_pct",
        "velocidade_vento_media_ms",
        "pressao_media_hpa",
        "precipitacao_lag0_mm"
      ),
    .(
      estacao_historica,
      ano_alvo,
      variavel,
      n_pares,
      spearman_bruto =
        spearman,
      n_pares_anomalia,
      spearman_anomalia_mensal,
      sinal_mantido
    )
  ]
)

cat("\nContraste MP10 em dias chuvosos versus secos:\n")

print(
  contraste_chuva[
    ,
    .(
      estacao_historica,
      ano_alvo,
      n_secos,
      n_chuvosos,
      mp10_media_seco,
      mp10_media_chuvoso,
      diferenca_media_chuvoso_menos_seco,
      razao_medias_chuvoso_seco
    )
  ]
)

cat(
  "\nIMPORTANTE:\n",
  "- Spearman e a medida principal; Pearson e complementar;\n",
  "- nao foram calculados p-valores inferenciais nesta etapa;\n",
  "- associacoes intramensais ajudam a separar co-variacao sazonal ampla;\n",
  "- precipitacao foi avaliada em lag 0, 1 e 2 dias;\n",
  "- direcao do vento nao foi tratada como variavel linear em graus;\n",
  "- cada variavel usa seus proprios pares validos;\n",
  "- resultados sao exploratorios e nao causais.\n",
  sep = ""
)
