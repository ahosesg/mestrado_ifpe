# ============================================================
# ETAPA 9N - AGREGACAO DIARIA DAS VARIAVEIS METEOROLOGICAS INMET
# Projeto de Mestrado - MP10 / Suape
# ============================================================
#
# OBJETIVO
# Converter a base horaria pareada da Etapa 9M em variaveis
# meteorologicas diarias compatíveis com as medias diarias de MP10.
#
# CRITERIO OPERACIONAL
# - 18 horas validas em 24 h (75%) = criterio diario principal;
# - 16 h e 20 h = analises de sensibilidade;
# - validade e avaliada separadamente por variavel;
# - nao e exigida disponibilidade simultanea de todas as variaveis
#   para que uma variavel individual seja utilizada;
# - o indicador "nuclear" existe apenas como diagnostico conjunto.
#
# AGREGACOES
# - temperatura: media, minima e maxima;
# - umidade relativa: media;
# - pressao: media;
# - velocidade do vento: media escalar e maxima;
# - direcao do vento: media circular;
# - vento vetorial: medias u/v, velocidade resultante e direcao
#   do vetor medio.
#
# REGRAS
# - sem imputacao;
# - principal e sensibilidade permanecem separadas;
# - dias inteiramente ausentes permanecem no calendario com zero
#   horas validas e valores analiticos NA;
# - SUAPE 2022 permanece fora ate resolver o pareamento.
# ============================================================

if (!requireNamespace("data.table", quietly = TRUE)) {
  stop("Instale o pacote 'data.table' antes de executar.")
}
library(data.table)

# ------------------------------------------------------------
# 1. PARAMETROS E CAMINHOS
# ------------------------------------------------------------

LIMIAR_PRINCIPAL <- 18L
LIMIAR_SENS_1 <- 16L
LIMIAR_SENS_2 <- 20L

ARQ_HORARIA <- file.path(
  "outputs", "09_meteorologia", "multivariaveis_inmet",
  "base_horaria_final",
  "base_horaria_inmet_pareada.rds"
)

ARQ_MP10 <- file.path(
  "outputs", "04_completude",
  "completude_diaria_mp10.csv"
)

SAIDA <- file.path(
  "outputs", "09_meteorologia", "multivariaveis_inmet",
  "base_diaria"
)

ARQ_RDS_DIARIA <- file.path(
  SAIDA,
  "base_diaria_inmet_pareada.rds"
)

dir.create(
  SAIDA,
  recursive = TRUE,
  showWarnings = FALSE
)

for (f in c(ARQ_HORARIA, ARQ_MP10)) {
  if (!file.exists(f)) {
    stop("Arquivo necessario nao encontrado: ", f)
  }
}

# ------------------------------------------------------------
# 2. FUNCOES
# ------------------------------------------------------------

media_segura <- function(x) {
  z <- x[is.finite(x)]
  if (!length(z)) return(NA_real_)
  mean(z)
}

min_seguro <- function(x) {
  z <- x[is.finite(x)]
  if (!length(z)) return(NA_real_)
  min(z)
}

max_seguro <- function(x) {
  z <- x[is.finite(x)]
  if (!length(z)) return(NA_real_)
  max(z)
}

direcao_circular_media <- function(graus) {
  z <- graus[is.finite(graus)]
  if (!length(z)) return(NA_real_)

  rad <- z * pi / 180
  s <- mean(sin(rad))
  c <- mean(cos(rad))

  ang <- atan2(s, c) * 180 / pi
  (ang + 360) %% 360
}

resultante_circular_r <- function(graus) {
  z <- graus[is.finite(graus)]
  if (!length(z)) return(NA_real_)

  rad <- z * pi / 180
  s <- mean(sin(rad))
  c <- mean(cos(rad))

  sqrt(s^2 + c^2)
}

direcao_de_uv <- function(u, v) {
  if (
    !is.finite(u) ||
      !is.finite(v)
  ) {
    return(NA_real_)
  }

  if (
    abs(u) < .Machine$double.eps &&
      abs(v) < .Machine$double.eps
  ) {
    return(NA_real_)
  }

  # Convencao meteorologica: direcao DE ONDE o vento sopra.
  ang <- atan2(-u, -v) * 180 / pi
  (ang + 360) %% 360
}

eh_bissexto <- function(ano) {
  (
    ano %% 400 == 0
  ) | (
    ano %% 4 == 0 &
      ano %% 100 != 0
  )
}

dias_esperados_ano <- function(ano) {
  ifelse(
    eh_bissexto(ano),
    366L,
    365L
  )
}

# ------------------------------------------------------------
# 3. CARREGAR BASE HORARIA
# ------------------------------------------------------------

cat("\nCarregando base horaria consolidada da Etapa 9M...\n")

horaria <- as.data.table(
  readRDS(
    ARQ_HORARIA
  )
)

necessarias <- c(
  "estacao_historica",
  "ano_alvo",
  "papel_serie",
  "cod_estacao",
  "nome_estacao_inmet",
  "distancia_km",
  "status_pareamento",
  "data_local",
  "datahora_local",
  "temperatura_ar_c_analitica",
  "umidade_relativa_pct_analitica",
  "velocidade_vento_ms_analitica",
  "direcao_vento_graus_analitica",
  "pressao_estacao_hpa_analitica",
  "vento_u_ms",
  "vento_v_ms",
  "vento_par_valido"
)

faltantes <- setdiff(
  necessarias,
  names(horaria)
)

if (length(faltantes)) {
  stop(
    "Colunas ausentes na base 9M: ",
    paste(
      faltantes,
      collapse = ", "
    )
  )
}

horaria[
  ,
  `:=`(
    estacao_historica = as.character(
      estacao_historica
    ),
    ano_alvo = as.integer(
      ano_alvo
    ),
    papel_serie = as.character(
      papel_serie
    ),
    cod_estacao = as.character(
      cod_estacao
    ),
    data_local = as.IDate(
      data_local
    )
  )
]

# ------------------------------------------------------------
# 4. AGREGACAO DOS DIAS OBSERVADOS
# ------------------------------------------------------------

cat("Agregando dias observados...\n")

diario_obs <- horaria[
  ,
  {
    temp <- temperatura_ar_c_analitica
    ur <- umidade_relativa_pct_analitica
    vel <- velocidade_vento_ms_analitica
    dir <- direcao_vento_graus_analitica
    press <- pressao_estacao_hpa_analitica
    u <- vento_u_ms
    v <- vento_v_ms

    n_temp <- sum(is.finite(temp))
    n_ur <- sum(is.finite(ur))
    n_vel <- sum(is.finite(vel))
    n_dir <- sum(is.finite(dir))
    n_press <- sum(is.finite(press))

    pares <- (
      is.finite(u) &
        is.finite(v)
    )

    n_pares <- sum(pares)

    u_med <- media_segura(u)
    v_med <- media_segura(v)

    list(
      n_timestamps = uniqueN(
        datahora_local[
          !is.na(datahora_local)
        ]
      ),
      n_temp_validas = n_temp,
      n_ur_validas = n_ur,
      n_vel_validas = n_vel,
      n_dir_validas = n_dir,
      n_press_validas = n_press,
      n_pares_vento_validos = n_pares,

      temperatura_media_bruta_c =
        media_segura(temp),
      temperatura_min_bruta_c =
        min_seguro(temp),
      temperatura_max_bruta_c =
        max_seguro(temp),

      umidade_media_bruta_pct =
        media_segura(ur),

      pressao_media_bruta_hpa =
        media_segura(press),

      velocidade_vento_media_bruta_ms =
        media_segura(vel),
      velocidade_vento_max_bruta_ms =
        max_seguro(vel),

      direcao_circular_media_bruta_graus =
        direcao_circular_media(dir),
      resultante_circular_r_bruta =
        resultante_circular_r(dir),

      vento_u_medio_bruto_ms =
        u_med,
      vento_v_medio_bruto_ms =
        v_med,

      velocidade_vetor_medio_bruta_ms =
        if (
          is.finite(u_med) &&
            is.finite(v_med)
        ) {
          sqrt(
            u_med^2 +
              v_med^2
          )
        } else {
          NA_real_
        },

      direcao_vetor_medio_bruta_graus =
        direcao_de_uv(
          u_med,
          v_med
        )
    )
  },
  by = .(
    estacao_historica,
    ano_alvo,
    papel_serie,
    cod_estacao,
    nome_estacao_inmet,
    distancia_km,
    status_pareamento,
    data_local
  )
]

# ------------------------------------------------------------
# 5. GRADE CALENDARIO COMPLETA
# ------------------------------------------------------------

cat("Completando grade diaria de calendario...\n")

series <- unique(
  horaria[
    ,
    .(
      estacao_historica,
      ano_alvo,
      papel_serie,
      cod_estacao,
      nome_estacao_inmet,
      distancia_km,
      status_pareamento
    )
  ]
)

calendario <- rbindlist(
  lapply(
    seq_len(nrow(series)),
    function(i) {
      a <- series$ano_alvo[i]

      data.table(
        estacao_historica =
          series$estacao_historica[i],
        ano_alvo =
          a,
        papel_serie =
          series$papel_serie[i],
        cod_estacao =
          series$cod_estacao[i],
        nome_estacao_inmet =
          series$nome_estacao_inmet[i],
        distancia_km =
          series$distancia_km[i],
        status_pareamento =
          series$status_pareamento[i],
        data_local = seq(
          as.IDate(
            sprintf(
              "%04d-01-01",
              a
            )
          ),
          as.IDate(
            sprintf(
              "%04d-12-31",
              a
            )
          ),
          by = "day"
        )
      )
    }
  )
)

diaria <- merge(
  calendario,
  diario_obs,
  by = c(
    "estacao_historica",
    "ano_alvo",
    "papel_serie",
    "cod_estacao",
    "nome_estacao_inmet",
    "distancia_km",
    "status_pareamento",
    "data_local"
  ),
  all.x = TRUE
)

contagens <- c(
  "n_timestamps",
  "n_temp_validas",
  "n_ur_validas",
  "n_vel_validas",
  "n_dir_validas",
  "n_press_validas",
  "n_pares_vento_validos"
)

for (col in contagens) {
  set(
    diaria,
    i = which(is.na(diaria[[col]])),
    j = col,
    value = 0L
  )
}

# ------------------------------------------------------------
# 6. FLAGS DE VALIDADE DIARIA
# ------------------------------------------------------------

cat("Aplicando criterios diarios 16/18/20 h...\n")

for (lim in c(
  LIMIAR_SENS_1,
  LIMIAR_PRINCIPAL,
  LIMIAR_SENS_2
)) {

  diaria[
    ,
    paste0(
      "temp_",
      lim,
      "h"
    ) := n_temp_validas >= lim
  ]

  diaria[
    ,
    paste0(
      "ur_",
      lim,
      "h"
    ) := n_ur_validas >= lim
  ]

  diaria[
    ,
    paste0(
      "vel_",
      lim,
      "h"
    ) := n_vel_validas >= lim
  ]

  diaria[
    ,
    paste0(
      "dir_",
      lim,
      "h"
    ) := n_dir_validas >= lim
  ]

  diaria[
    ,
    paste0(
      "press_",
      lim,
      "h"
    ) := n_press_validas >= lim
  ]

  diaria[
    ,
    paste0(
      "vetor_",
      lim,
      "h"
    ) := n_pares_vento_validos >= lim
  ]

  diaria[
    ,
    paste0(
      "nuclear_",
      lim,
      "h"
    ) := (
      n_temp_validas >= lim &
        n_ur_validas >= lim &
        n_vel_validas >= lim &
        n_dir_validas >= lim
    )
  ]
}

# ------------------------------------------------------------
# 7. VARIAVEIS DIARIAS ANALITICAS, CRITERIO PRINCIPAL 18 H
# ------------------------------------------------------------

diaria[
  ,
  temperatura_media_c := fifelse(
    temp_18h,
    temperatura_media_bruta_c,
    NA_real_
  )
]

diaria[
  ,
  temperatura_min_c := fifelse(
    temp_18h,
    temperatura_min_bruta_c,
    NA_real_
  )
]

diaria[
  ,
  temperatura_max_c := fifelse(
    temp_18h,
    temperatura_max_bruta_c,
    NA_real_
  )
]

diaria[
  ,
  umidade_media_pct := fifelse(
    ur_18h,
    umidade_media_bruta_pct,
    NA_real_
  )
]

diaria[
  ,
  pressao_media_hpa := fifelse(
    press_18h,
    pressao_media_bruta_hpa,
    NA_real_
  )
]

diaria[
  ,
  velocidade_vento_media_ms := fifelse(
    vel_18h,
    velocidade_vento_media_bruta_ms,
    NA_real_
  )
]

diaria[
  ,
  velocidade_vento_max_ms := fifelse(
    vel_18h,
    velocidade_vento_max_bruta_ms,
    NA_real_
  )
]

diaria[
  ,
  direcao_circular_media_graus := fifelse(
    dir_18h,
    direcao_circular_media_bruta_graus,
    NA_real_
  )
]

diaria[
  ,
  resultante_circular_r := fifelse(
    dir_18h,
    resultante_circular_r_bruta,
    NA_real_
  )
]

diaria[
  ,
  vento_u_medio_ms := fifelse(
    vetor_18h,
    vento_u_medio_bruto_ms,
    NA_real_
  )
]

diaria[
  ,
  vento_v_medio_ms := fifelse(
    vetor_18h,
    vento_v_medio_bruto_ms,
    NA_real_
  )
]

diaria[
  ,
  velocidade_vetor_medio_ms := fifelse(
    vetor_18h,
    velocidade_vetor_medio_bruta_ms,
    NA_real_
  )
]

diaria[
  ,
  direcao_vetor_medio_graus := fifelse(
    vetor_18h,
    direcao_vetor_medio_bruta_graus,
    NA_real_
  )
]

diaria[
  ,
  criterio_diario_principal_horas :=
    LIMIAR_PRINCIPAL
]

diaria[
  ,
  fonte_meteorologica := "INMET"
]

diaria[
  ,
  imputacao := FALSE
]

setorder(
  diaria,
  estacao_historica,
  ano_alvo,
  papel_serie,
  data_local
)

# ------------------------------------------------------------
# 8. AUDITORIA DA GRADE DIARIA
# ------------------------------------------------------------

aud_grade <- diaria[
  ,
  .(
    n_dias_grade = .N,
    n_datas_unicas = uniqueN(
      data_local
    ),
    n_dias_esperados =
      dias_esperados_ano(
        unique(ano_alvo)
      ),
    primeira_data =
      min(data_local),
    ultima_data =
      max(data_local)
  ),
  by = .(
    estacao_historica,
    ano_alvo,
    papel_serie,
    cod_estacao
  )
]

aud_grade[
  ,
  grade_ok := (
    n_dias_grade ==
      n_dias_esperados &
      n_datas_unicas ==
        n_dias_esperados
  )
]

fwrite(
  aud_grade,
  file.path(
    SAIDA,
    "auditoria_grade_diaria.csv"
  ),
  bom = TRUE
)

if (any(!aud_grade$grade_ok)) {
  stop(
    "A grade diaria nao cobre integralmente o calendario de uma ou mais series."
  )
}

# ------------------------------------------------------------
# 9. RESUMO DE VALIDADE DIARIA
# ------------------------------------------------------------

resumo <- diaria[
  ,
  .(
    n_dias_calendario = .N,

    n_dias_temp_16h = sum(
      temp_16h
    ),
    n_dias_temp_18h = sum(
      temp_18h
    ),
    n_dias_temp_20h = sum(
      temp_20h
    ),

    n_dias_ur_16h = sum(
      ur_16h
    ),
    n_dias_ur_18h = sum(
      ur_18h
    ),
    n_dias_ur_20h = sum(
      ur_20h
    ),

    n_dias_vel_16h = sum(
      vel_16h
    ),
    n_dias_vel_18h = sum(
      vel_18h
    ),
    n_dias_vel_20h = sum(
      vel_20h
    ),

    n_dias_dir_16h = sum(
      dir_16h
    ),
    n_dias_dir_18h = sum(
      dir_18h
    ),
    n_dias_dir_20h = sum(
      dir_20h
    ),

    n_dias_press_16h = sum(
      press_16h
    ),
    n_dias_press_18h = sum(
      press_18h
    ),
    n_dias_press_20h = sum(
      press_20h
    ),

    n_dias_vetor_16h = sum(
      vetor_16h
    ),
    n_dias_vetor_18h = sum(
      vetor_18h
    ),
    n_dias_vetor_20h = sum(
      vetor_20h
    ),

    n_dias_nuclear_16h = sum(
      nuclear_16h
    ),
    n_dias_nuclear_18h = sum(
      nuclear_18h
    ),
    n_dias_nuclear_20h = sum(
      nuclear_20h
    )
  ),
  by = .(
    estacao_historica,
    ano_alvo,
    papel_serie,
    cod_estacao,
    nome_estacao_inmet,
    distancia_km
  )
]

# Percentuais do criterio principal.
for (v in c(
  "temp",
  "ur",
  "vel",
  "dir",
  "press",
  "vetor",
  "nuclear"
)) {
  nome_n <- paste0(
    "n_dias_",
    v,
    "_18h"
  )
  nome_pct <- paste0(
    "pct_dias_",
    v,
    "_18h"
  )

  resumo[
    ,
    (nome_pct) :=
      100 *
        get(nome_n) /
        n_dias_calendario
  ]
}

fwrite(
  resumo,
  file.path(
    SAIDA,
    "resumo_validade_diaria_inmet.csv"
  ),
  bom = TRUE
)

# ------------------------------------------------------------
# 10. SOBREPOSICAO COM DIAS VALIDOS DE MP10
# ------------------------------------------------------------

cat("Calculando sobreposicao diaria por variavel com MP10...\n")

mp10 <- fread(
  ARQ_MP10,
  encoding = "UTF-8"
)

necessarias_mp10 <- c(
  "ano",
  "no_estacao",
  "data",
  "dia_valido_mma_16h",
  "media_24h_mma"
)

faltantes_mp10 <- setdiff(
  necessarias_mp10,
  names(mp10)
)

if (length(faltantes_mp10)) {
  stop(
    "Colunas ausentes em completude_diaria_mp10.csv: ",
    paste(
      faltantes_mp10,
      collapse = ", "
    )
  )
}

mp10[
  ,
  `:=`(
    ano = as.integer(
      ano
    ),
    estacao_historica = as.character(
      no_estacao
    ),
    data_local = as.IDate(
      data
    )
  )
]

integrada <- merge(
  diaria,
  mp10[
    ,
    .(
      estacao_historica,
      ano_alvo = ano,
      data_local,
      dia_mp10_valido =
        dia_valido_mma_16h,
      mp10_media_24h =
        media_24h_mma
    )
  ],
  by = c(
    "estacao_historica",
    "ano_alvo",
    "data_local"
  ),
  all.x = TRUE
)

# Todos os alvos da base diaria devem existir na grade de MP10.
aud_mp10 <- integrada[
  ,
  .(
    n_dias = .N,
    n_dias_com_flag_mp10 = sum(
      !is.na(
        dia_mp10_valido
      )
    )
  ),
  by = .(
    estacao_historica,
    ano_alvo,
    papel_serie,
    cod_estacao
  )
]

aud_mp10[
  ,
  mp10_grade_ok := (
    n_dias ==
      n_dias_com_flag_mp10
  )
]

fwrite(
  aud_mp10,
  file.path(
    SAIDA,
    "auditoria_integracao_mp10.csv"
  ),
  bom = TRUE
)

if (any(!aud_mp10$mp10_grade_ok)) {
  stop(
    "Uma ou mais series meteorologicas nao encontraram a grade diaria completa de MP10."
  )
}

sobreposicao <- integrada[
  ,
  {
    valido_mp10 <- (
      dia_mp10_valido %in% TRUE
    )

    n_mp10 <- sum(
      valido_mp10
    )

    calc <- function(flag) {
      n <- sum(
        valido_mp10 &
          flag,
        na.rm = TRUE
      )

      pct <- if (
        n_mp10 > 0
      ) {
        100 * n / n_mp10
      } else {
        NA_real_
      }

      c(
        n = n,
        pct = pct
      )
    }

    a <- calc(temp_18h)
    b <- calc(ur_18h)
    c1 <- calc(vel_18h)
    d <- calc(dir_18h)
    e <- calc(press_18h)
    f <- calc(vetor_18h)
    g <- calc(nuclear_18h)

    list(
      n_dias_mp10_validos = n_mp10,

      n_overlap_temp_18h = a["n"],
      pct_overlap_temp_18h = a["pct"],

      n_overlap_ur_18h = b["n"],
      pct_overlap_ur_18h = b["pct"],

      n_overlap_vel_18h = c1["n"],
      pct_overlap_vel_18h = c1["pct"],

      n_overlap_dir_18h = d["n"],
      pct_overlap_dir_18h = d["pct"],

      n_overlap_press_18h = e["n"],
      pct_overlap_press_18h = e["pct"],

      n_overlap_vetor_18h = f["n"],
      pct_overlap_vetor_18h = f["pct"],

      n_overlap_nuclear_18h = g["n"],
      pct_overlap_nuclear_18h = g["pct"]
    )
  },
  by = .(
    estacao_historica,
    ano_alvo,
    papel_serie,
    cod_estacao,
    nome_estacao_inmet,
    distancia_km
  )
]

fwrite(
  sobreposicao,
  file.path(
    SAIDA,
    "sobreposicao_diaria_mp10_inmet.csv"
  ),
  bom = TRUE
)

# ------------------------------------------------------------
# 11. EXPORTAR BASES
# ------------------------------------------------------------

saveRDS(
  diaria,
  ARQ_RDS_DIARIA,
  compress = "xz"
)

# CSV completo e pequeno o suficiente para auditoria/versionamento.
fwrite(
  diaria,
  file.path(
    SAIDA,
    "base_diaria_inmet_pareada.csv"
  ),
  bom = TRUE
)

# Base integrada com MP10, útil para a futura análise de associação.
fwrite(
  integrada,
  file.path(
    SAIDA,
    "base_diaria_inmet_mp10_integrada.csv"
  ),
  bom = TRUE
)

# ------------------------------------------------------------
# 12. DOCUMENTAR CRITERIOS
# ------------------------------------------------------------

criterios <- data.table(
  componente = c(
    "criterio_principal",
    "sensibilidade_inferior",
    "sensibilidade_superior",
    "temperatura",
    "umidade",
    "pressao",
    "velocidade_vento",
    "direcao_vento",
    "vento_vetorial",
    "validade_conjunta",
    "imputacao"
  ),
  regra = c(
    ">=18 horas validas no dia (75%); criterio operacional do projeto",
    ">=16 horas validas no dia; sensibilidade",
    ">=20 horas validas no dia; sensibilidade",
    "media, minima e maxima; validade avaliada pela propria variavel",
    "media; validade avaliada pela propria variavel",
    "media; validade avaliada pela propria variavel",
    "media escalar e maxima; validade avaliada pela velocidade",
    "media circular; nao usar media aritmetica simples de graus",
    "medias u/v e vetor medio; requer pares velocidade-direcao validos",
    "indicador nuclear apenas diagnostico; nao filtra automaticamente variaveis individuais",
    "nenhuma imputacao entre horas, dias ou estacoes"
  )
)

fwrite(
  criterios,
  file.path(
    SAIDA,
    "criterios_agregacao_diaria_inmet.csv"
  ),
  bom = TRUE
)

# ------------------------------------------------------------
# 13. RESUMO EXECUCAO
# ------------------------------------------------------------

resumo_exec <- data.table(
  indicador = c(
    "n_series_diarias",
    "n_series_principais",
    "n_series_sensibilidade",
    "n_linhas_calendario_diario",
    "n_grades_calendario_ok",
    "n_integracoes_mp10_ok",
    "criterio_principal_horas",
    "n_valores_imputados"
  ),
  valor = c(
    nrow(series),
    sum(
      series$papel_serie ==
        "principal"
    ),
    sum(
      series$papel_serie ==
        "sensibilidade"
    ),
    nrow(diaria),
    sum(
      aud_grade$grade_ok
    ),
    sum(
      aud_mp10$mp10_grade_ok
    ),
    LIMIAR_PRINCIPAL,
    0L
  )
)

fwrite(
  resumo_exec,
  file.path(
    SAIDA,
    "resumo_execucao_agregacao_diaria_inmet.csv"
  ),
  bom = TRUE
)

# ------------------------------------------------------------
# 14. CONSOLE
# ------------------------------------------------------------

cat("\n============================================================\n")
cat("ETAPA 9N - BASE DIARIA INMET CONCLUIDA\n")
cat("============================================================\n\n")

print(resumo_exec)

cat("\nSeries principais, validade diaria no criterio de 18 h:\n")

print(
  resumo[
    papel_serie == "principal",
    .(
      estacao_historica,
      ano_alvo,
      cod_estacao,
      nome_estacao_inmet,
      distancia_km,
      n_dias_calendario,
      n_dias_temp_18h,
      n_dias_ur_18h,
      n_dias_vel_18h,
      n_dias_dir_18h,
      n_dias_press_18h,
      n_dias_vetor_18h,
      n_dias_nuclear_18h
    )
  ]
)

cat("\nSobreposicao das series principais com dias validos de MP10:\n")

print(
  sobreposicao[
    papel_serie == "principal",
    .(
      estacao_historica,
      ano_alvo,
      cod_estacao,
      n_dias_mp10_validos,
      pct_overlap_temp_18h,
      pct_overlap_ur_18h,
      pct_overlap_vel_18h,
      pct_overlap_dir_18h,
      pct_overlap_press_18h,
      pct_overlap_vetor_18h,
      pct_overlap_nuclear_18h
    )
  ]
)

cat(
  "\nREGRAS:\n",
  "- 18 h e o criterio diario operacional principal; 16/20 h ficam como sensibilidade;\n",
  "- a validade e especifica por variavel;\n",
  "- vento direcional usa estatistica circular e componentes vetoriais;\n",
  "- nao houve imputacao;\n",
  "- SUAPE 2022 continua pendente e nao integra esta base.\n",
  sep = ""
)
