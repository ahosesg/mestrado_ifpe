# ============================================================
# ETAPA 9O - CONSOLIDACAO FINAL DA BASE METEOROLOGICA
# Projeto de Mestrado - MP10 / Suape
# ============================================================
#
# OBJETIVO
# Encerrar a Etapa 9 com:
# 1) matriz consolidada dos pareamentos meteorologicos;
# 2) base diaria principal integrada MP10 + INMET + precipitacao;
# 3) resumo de cobertura por variavel;
# 4) registro de limitacoes e pendencias metodologicas.
#
# FONTES CONGELADAS
# - temperatura, umidade, vento e pressao: INMET;
# - precipitacao: CEMADEN;
# - MP10: base diaria validada na Etapa 4.
#
# REGRAS
# - sem imputacao;
# - sem preenchimento entre estacoes;
# - series de sensibilidade nao completam a principal;
# - INMET diario principal: >=18 h validas; 16/20 h sensibilidade;
# - precipitacao CEMADEN principal: continuidade com gap maximo 60 min;
#   70/90 min permanecem como sensibilidade;
# - direcao do vento tratada circularmente;
# - SUAPE 2022 permanece pendente de coordenada historica valida.
# ============================================================

if (!requireNamespace("data.table", quietly = TRUE)) {
  stop("Instale o pacote 'data.table' antes de executar.")
}
library(data.table)

# ------------------------------------------------------------
# 1. CAMINHOS
# ------------------------------------------------------------

ARQ_INMET <- file.path(
  "outputs", "09_meteorologia", "multivariaveis_inmet",
  "base_diaria",
  "base_diaria_inmet_mp10_integrada.csv"
)

ARQ_PAREAMENTOS_INMET <- file.path(
  "outputs", "09_meteorologia", "multivariaveis_inmet",
  "pareamentos_finais",
  "pareamentos_inmet_finais.csv"
)

ARQ_PRECIP_09F <- file.path(
  "outputs", "09_meteorologia", "precipitacao",
  "convencao_diaria_final",
  "series_diarias_precipitacao_convencao_final.csv"
)

ARQ_PRECIP_GAIBU <- file.path(
  "outputs", "09_meteorologia", "precipitacao",
  "gaibu_2019", "series_diarias",
  "series_diarias_gaibu_2019.csv"
)

SAIDA <- file.path(
  "outputs", "09_meteorologia",
  "consolidacao_final"
)

dir.create(
  SAIDA,
  recursive = TRUE,
  showWarnings = FALSE
)

for (f in c(
  ARQ_INMET,
  ARQ_PAREAMENTOS_INMET,
  ARQ_PRECIP_09F,
  ARQ_PRECIP_GAIBU
)) {
  if (!file.exists(f)) {
    stop("Arquivo necessario nao encontrado: ", f)
  }
}

# ------------------------------------------------------------
# 2. LER INSUMOS
# ------------------------------------------------------------

cat("\nCarregando produtos finais das subetapas meteorologicas...\n")

inmet <- fread(
  ARQ_INMET,
  encoding = "UTF-8"
)

pare_inmet <- fread(
  ARQ_PAREAMENTOS_INMET,
  encoding = "UTF-8"
)

precip_09f <- fread(
  ARQ_PRECIP_09F,
  encoding = "UTF-8"
)

precip_gaibu <- fread(
  ARQ_PRECIP_GAIBU,
  encoding = "UTF-8"
)

# ------------------------------------------------------------
# 3. NORMALIZAR CHAVES
# ------------------------------------------------------------

inmet[
  ,
  `:=`(
    estacao_historica =
      as.character(estacao_historica),
    ano_alvo =
      as.integer(ano_alvo),
    papel_serie =
      as.character(papel_serie),
    data_local =
      as.IDate(data_local)
  )
]

pare_inmet[
  ,
  `:=`(
    estacao_historica =
      as.character(estacao_historica),
    ano_alvo =
      as.integer(ano_alvo),
    cod_principal =
      trimws(as.character(cod_principal)),
    cod_sensibilidade =
      trimws(as.character(cod_sensibilidade))
  )
]

pare_inmet[
  cod_principal == "",
  cod_principal := NA_character_
]

pare_inmet[
  cod_sensibilidade == "",
  cod_sensibilidade := NA_character_
]

precip_09f[
  ,
  `:=`(
    estacao_historica =
      as.character(estacao_historica),
    ano_alvo =
      as.integer(ano_alvo),
    cod_estacao =
      as.character(cod_estacao),
    data_local =
      as.IDate(data_local)
  )
]

precip_gaibu[
  ,
  `:=`(
    cod_estacao =
      as.character(cod_estacao),
    data_local =
      as.IDate(data_local)
  )
]

# ------------------------------------------------------------
# 4. PAREAMENTOS FINAIS DE PRECIPITACAO
# ------------------------------------------------------------

pare_precip <- data.table(
  estacao_historica = c(
    "IFPE",
    "Gaibu",
    "CUPE",
    "IPOJUCA",
    "IPOJUCA",
    "SUAPE"
  ),
  ano_alvo = c(
    2018L,
    2019L,
    2021L,
    2021L,
    2025L,
    2022L
  ),
  cod_precip_principal = c(
    "260720801A",
    "260290204A",
    "260720801A",
    "260720801A",
    "260720804A",
    NA_character_
  ),
  nome_precip_principal = c(
    "Centro",
    "Enseado dos Corais",
    "Centro",
    "Centro",
    "Rurópolis",
    NA_character_
  ),
  distancia_precip_principal_km = c(
    2.94037517011968,
    0.116707872718135,
    2.31199229299342,
    7.32836097674827,
    7.14376732272583,
    NA_real_
  ),
  cod_precip_sensibilidade = c(
    "260720804A",
    "260790109A",
    "260720804A",
    "260720804A",
    "260720801G",
    NA_character_
  ),
  nome_precip_sensibilidade = c(
    "Rurópolis",
    "Barra de Jangada",
    "Rurópolis",
    "Rurópolis",
    "Campo do Avião",
    NA_character_
  ),
  distancia_precip_sensibilidade_km = c(
    4.23417961072742,
    12.4106755442896,
    3.31306361103069,
    7.14376732272583,
    6.92640747959743,
    NA_real_
  ),
  status_precipitacao = c(
    "fechado",
    "fechado",
    "fechado_com_proxy_espacial_CUPE",
    "fechado",
    "fechado_com_limitacao_borda_2025",
    "pendente_coordenada_historica"
  )
)

# ------------------------------------------------------------
# 5. EXTRAIR PRECIPITACAO DOS PAREAMENTOS CONGELADOS
# ------------------------------------------------------------

extrair_09f <- function(
  alvo,
  ano,
  codigo,
  papel
) {

  z <- precip_09f[
    estacao_historica == alvo &
      ano_alvo == ano &
      cod_estacao == codigo
  ]

  if (!nrow(z)) {
    stop(
      "Serie de precipitacao 09F nao encontrada: ",
      alvo, " ", ano, " ", codigo
    )
  }

  z[
    ,
    .(
      estacao_historica =
        alvo,
      ano_alvo =
        ano,
      data_local,
      papel_precipitacao =
        papel,
      cod_precipitacao =
        codigo,
      nome_precipitacao =
        as.character(nome_estacao),
      distancia_precipitacao_km =
        as.numeric(distancia_km),
      precip_diaria_bruta_mm =
        as.numeric(precip_diaria_final_mm),
      precip_valida_60 =
        cobertura_60 %in% TRUE,
      precip_valida_70 =
        cobertura_70 %in% TRUE,
      precip_valida_90 =
        cobertura_90 %in% TRUE,
      flag_borda_2025 =
        flag_borda_2025 %in% TRUE
    )
  ]
}

extrair_gaibu <- function(
  codigo,
  papel
) {

  z <- precip_gaibu[
    cod_estacao == codigo
  ]

  if (!nrow(z)) {
    stop(
      "Serie de precipitacao Gaibu 2019 nao encontrada: ",
      codigo
    )
  }

  z[
    ,
    .(
      estacao_historica =
        "Gaibu",
      ano_alvo =
        2019L,
      data_local,
      papel_precipitacao =
        papel,
      cod_precipitacao =
        codigo,
      nome_precipitacao =
        as.character(nome_estacao),
      distancia_precipitacao_km =
        as.numeric(distancia_gaibu_km),
      precip_diaria_bruta_mm =
        as.numeric(precip_diaria_mm),
      precip_valida_60 =
        cobertura_60 %in% TRUE,
      precip_valida_70 =
        cobertura_70 %in% TRUE,
      precip_valida_90 =
        cobertura_90 %in% TRUE,
      flag_borda_2025 =
        FALSE
    )
  ]
}

precip_long <- rbindlist(
  list(
    extrair_09f(
      "IFPE", 2018L,
      "260720801A",
      "principal"
    ),
    extrair_09f(
      "IFPE", 2018L,
      "260720804A",
      "sensibilidade"
    ),
    extrair_gaibu(
      "260290204A",
      "principal"
    ),
    extrair_gaibu(
      "260790109A",
      "sensibilidade"
    ),
    extrair_09f(
      "CUPE", 2021L,
      "260720801A",
      "principal"
    ),
    extrair_09f(
      "CUPE", 2021L,
      "260720804A",
      "sensibilidade"
    ),
    extrair_09f(
      "IPOJUCA", 2021L,
      "260720801A",
      "principal"
    ),
    extrair_09f(
      "IPOJUCA", 2021L,
      "260720804A",
      "sensibilidade"
    ),
    extrair_09f(
      "IPOJUCA", 2025L,
      "260720804A",
      "principal"
    ),
    extrair_09f(
      "IPOJUCA", 2025L,
      "260720801G",
      "sensibilidade"
    )
  ),
  fill = TRUE
)

precip_long[
  ,
  precip_diaria_analitica_mm := fifelse(
    precip_valida_60,
    precip_diaria_bruta_mm,
    NA_real_
  )
]

setorder(
  precip_long,
  estacao_historica,
  ano_alvo,
  papel_precipitacao,
  data_local
)

fwrite(
  precip_long,
  file.path(
    SAIDA,
    "precipitacao_pareada_final_long.csv"
  ),
  bom = TRUE
)

# ------------------------------------------------------------
# 6. AUDITORIA DAS GRADES DE PRECIPITACAO
# ------------------------------------------------------------

aud_precip <- precip_long[
  ,
  .(
    n_dias = .N,
    n_datas_unicas =
      uniqueN(data_local),
    primeira_data =
      min(data_local),
    ultima_data =
      max(data_local),
    n_dias_validos_60 =
      sum(precip_valida_60),
    n_dias_validos_70 =
      sum(precip_valida_70),
    n_dias_validos_90 =
      sum(precip_valida_90)
  ),
  by = .(
    estacao_historica,
    ano_alvo,
    papel_precipitacao,
    cod_precipitacao,
    nome_precipitacao
  )
]

aud_precip[
  ,
  grade_ok := (
    n_dias == 365L &
      n_datas_unicas == 365L
  )
]

fwrite(
  aud_precip,
  file.path(
    SAIDA,
    "auditoria_precipitacao_pareada.csv"
  ),
  bom = TRUE
)

if (any(!aud_precip$grade_ok)) {
  stop(
    "Uma ou mais series pareadas de precipitacao nao possuem grade anual completa."
  )
}

# ------------------------------------------------------------
# 7. BASE PRINCIPAL INMET + MP10
# ------------------------------------------------------------

cat("Construindo base diaria principal integrada...\n")

principal <- inmet[
  papel_serie == "principal"
]

if (!nrow(principal)) {
  stop("Nenhuma serie principal INMET encontrada na base 9N.")
}

# Auditoria de unicidade.
dups_principal <- principal[
  ,
  .N,
  by = .(
    estacao_historica,
    ano_alvo,
    data_local
  )
][
  N != 1L
]

if (nrow(dups_principal)) {
  print(dups_principal)
  stop(
    "A base principal INMET nao e unica por alvo-ano-data."
  )
}

# ------------------------------------------------------------
# 8. ABRIR PRECIPITACAO PRINCIPAL E SENSIBILIDADE EM COLUNAS
# ------------------------------------------------------------

precip_principal <- precip_long[
  papel_precipitacao == "principal",
  .(
    estacao_historica,
    ano_alvo,
    data_local,
    cod_precip_principal =
      cod_precipitacao,
    nome_precip_principal =
      nome_precipitacao,
    distancia_precip_principal_km =
      distancia_precipitacao_km,
    precip_principal_bruta_mm =
      precip_diaria_bruta_mm,
    precip_principal_mm =
      precip_diaria_analitica_mm,
    precip_principal_valida_60 =
      precip_valida_60,
    precip_principal_valida_70 =
      precip_valida_70,
    precip_principal_valida_90 =
      precip_valida_90,
    precip_principal_borda_2025 =
      flag_borda_2025
  )
]

precip_sens <- precip_long[
  papel_precipitacao == "sensibilidade",
  .(
    estacao_historica,
    ano_alvo,
    data_local,
    cod_precip_sensibilidade =
      cod_precipitacao,
    nome_precip_sensibilidade =
      nome_precipitacao,
    distancia_precip_sensibilidade_km =
      distancia_precipitacao_km,
    precip_sens_bruta_mm =
      precip_diaria_bruta_mm,
    precip_sens_mm =
      precip_diaria_analitica_mm,
    precip_sens_valida_60 =
      precip_valida_60,
    precip_sens_valida_70 =
      precip_valida_70,
    precip_sens_valida_90 =
      precip_valida_90,
    precip_sens_borda_2025 =
      flag_borda_2025
  )
]

base_final <- merge(
  principal,
  precip_principal,
  by = c(
    "estacao_historica",
    "ano_alvo",
    "data_local"
  ),
  all.x = TRUE
)

base_final <- merge(
  base_final,
  precip_sens,
  by = c(
    "estacao_historica",
    "ano_alvo",
    "data_local"
  ),
  all.x = TRUE
)

# ------------------------------------------------------------
# 9. AUDITORIA DE INTEGRACAO
# ------------------------------------------------------------

aud_integracao <- base_final[
  ,
  .(
    n_dias = .N,
    n_datas_unicas =
      uniqueN(data_local),
    n_dias_com_precip_principal =
      sum(!is.na(cod_precip_principal)),
    n_dias_com_precip_sens =
      sum(!is.na(cod_precip_sensibilidade)),
    n_dias_mp10_validos =
      sum(dia_mp10_valido %in% TRUE)
  ),
  by = .(
    estacao_historica,
    ano_alvo,
    cod_estacao,
    nome_estacao_inmet
  )
]

aud_integracao[
  ,
  integracao_ok := (
    n_dias == 365L &
      n_datas_unicas == 365L &
      n_dias_com_precip_principal == 365L &
      n_dias_com_precip_sens == 365L
  )
]

fwrite(
  aud_integracao,
  file.path(
    SAIDA,
    "auditoria_base_diaria_integrada.csv"
  ),
  bom = TRUE
)

if (any(!aud_integracao$integracao_ok)) {
  print(
    aud_integracao[
      integracao_ok == FALSE
    ]
  )
  stop(
    "A integracao final possui grade incompleta em um ou mais alvos."
  )
}

# ------------------------------------------------------------
# 10. COBERTURA FINAL SOBRE DIAS VALIDOS DE MP10
# ------------------------------------------------------------

cobertura <- base_final[
  ,
  {
    mp <- dia_mp10_valido %in% TRUE
    n_mp <- sum(mp)

    calc <- function(flag) {
      n <- sum(
        mp &
          flag,
        na.rm = TRUE
      )
      pct <- if (
        n_mp > 0
      ) {
        100 * n / n_mp
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
    g <- calc(precip_principal_valida_60)
    h <- calc(precip_sens_valida_60)

    list(
      n_dias_mp10_validos =
        n_mp,

      n_temp =
        a["n"],
      pct_temp =
        a["pct"],

      n_ur =
        b["n"],
      pct_ur =
        b["pct"],

      n_vel =
        c1["n"],
      pct_vel =
        c1["pct"],

      n_dir =
        d["n"],
      pct_dir =
        d["pct"],

      n_press =
        e["n"],
      pct_press =
        e["pct"],

      n_vetor =
        f["n"],
      pct_vetor =
        f["pct"],

      n_precip_principal =
        g["n"],
      pct_precip_principal =
        g["pct"],

      n_precip_sens =
        h["n"],
      pct_precip_sens =
        h["pct"]
    )
  },
  by = .(
    estacao_historica,
    ano_alvo,
    cod_inmet =
      cod_estacao,
    nome_inmet =
      nome_estacao_inmet,
    distancia_inmet_km =
      distancia_km,
    cod_precip_principal,
    nome_precip_principal,
    distancia_precip_principal_km
  )
]

fwrite(
  cobertura,
  file.path(
    SAIDA,
    "cobertura_final_por_variavel.csv"
  ),
  bom = TRUE
)

# ------------------------------------------------------------
# 11. MATRIZ CONSOLIDADA DE PAREAMENTOS
# ------------------------------------------------------------

pare_consolidado <- merge(
  pare_inmet[
    ,
    .(
      estacao_historica,
      ano_alvo,
      cod_inmet_principal =
        cod_principal,
      nome_inmet_principal =
        nome_principal,
      distancia_inmet_principal_km =
        distancia_principal_km,
      cod_inmet_sensibilidade =
        cod_sensibilidade,
      nome_inmet_sensibilidade =
        nome_sensibilidade,
      distancia_inmet_sensibilidade_km =
        distancia_sensibilidade_km,
      status_inmet =
        status_pareamento
    )
  ],
  pare_precip,
  by = c(
    "estacao_historica",
    "ano_alvo"
  ),
  all = TRUE
)

pare_consolidado[
  ,
  fonte_multivariavel := fifelse(
    is.na(cod_inmet_principal),
    NA_character_,
    "INMET"
  )
]

pare_consolidado[
  ,
  fonte_precipitacao := fifelse(
    is.na(cod_precip_principal),
    NA_character_,
    "CEMADEN"
  )
]

setorder(
  pare_consolidado,
  ano_alvo,
  estacao_historica
)

fwrite(
  pare_consolidado,
  file.path(
    SAIDA,
    "pareamentos_meteorologicos_consolidados.csv"
  ),
  bom = TRUE
)

# ------------------------------------------------------------
# 12. LIMITACOES CONSOLIDADAS
# ------------------------------------------------------------

limitacoes <- data.table(
  estacao_historica = c(
    "IFPE",
    "Gaibu",
    "CUPE",
    "IPOJUCA",
    "IPOJUCA",
    "SUAPE"
  ),
  ano_alvo = c(
    2018L,
    2019L,
    2021L,
    2021L,
    2025L,
    2022L
  ),
  limitacao = c(
    paste(
      "INMET principal a cerca de 37 km; interpretar temperatura,",
      "umidade, vento e pressao como condicoes meteorologicas regionais."
    ),
    paste(
      "INMET A301 apresenta cobertura reduzida de vento em 2019;",
      "a precipitacao principal e local, mas possui cobertura propria",
      "definida pelo criterio de continuidade de 60 min."
    ),
    paste(
      "A301 encerra registros em novembro de 2021; coordenada do alvo",
      "CUPE utiliza EDCUPE 2024 como proxy espacial."
    ),
    paste(
      "A301 encerra registros em novembro de 2021 e esta a cerca de",
      "43 km da referencia espacial adotada para IPOJUCA."
    ),
    paste(
      "INMET A357 esta a cerca de 66 km e a umidade relativa possui",
      "menor cobertura; precipitacao de 31/12/2025 possui limitacao",
      "de borda pela ausencia de transmissao de 01/01/2026."
    ),
    paste(
      "Coordenada historica valida de SUAPE/PE06 ainda nao foi obtida;",
      "pareamentos meteorologicos permanecem pendentes."
    )
  )
)

fwrite(
  limitacoes,
  file.path(
    SAIDA,
    "limitacoes_meteorologicas_consolidadas.csv"
  ),
  bom = TRUE
)

# ------------------------------------------------------------
# 13. BASE FINAL E RDS
# ------------------------------------------------------------

setorder(
  base_final,
  estacao_historica,
  ano_alvo,
  data_local
)

fwrite(
  base_final,
  file.path(
    SAIDA,
    "base_diaria_principal_mp10_meteorologia.csv"
  ),
  bom = TRUE
)

saveRDS(
  base_final,
  file.path(
    SAIDA,
    "base_diaria_principal_mp10_meteorologia.rds"
  ),
  compress = "xz"
)

# ------------------------------------------------------------
# 14. DOCUMENTACAO METODOLOGICA
# ------------------------------------------------------------

linhas_md <- c(
  "# Consolidação metodológica da Etapa 9",
  "",
  "## Fontes",
  "",
  "Temperatura do ar, umidade relativa, velocidade e direção do vento e pressão atmosférica são provenientes exclusivamente de estações automáticas do INMET. A precipitação é proveniente da rede CEMADEN, conforme os pareamentos avaliados nas subetapas 9D a 9H.",
  "",
  "## Regras de validade",
  "",
  "Para as variáveis horárias do INMET, adotou-se operacionalmente como critério principal de agregação diária a disponibilidade de pelo menos 18 horas válidas em 24 horas (75%). Os limiares de 16 e 20 horas são preservados como análises de sensibilidade. A validade é específica para cada variável, de modo que a indisponibilidade de uma variável não invalida automaticamente as demais.",
  "",
  "A direção do vento é tratada como variável circular. Foram preservadas a direção média circular e as componentes vetoriais u e v, evitando a média aritmética simples dos ângulos.",
  "",
  "Para a precipitação CEMADEN, a regra principal utiliza continuidade diária com maior intervalo entre registros de até 60 minutos, mantendo 70 e 90 minutos como sensibilidades. Registros exatamente à 00:00 local são atribuídos ao dia civil anterior, conforme a convenção temporal congelada na Etapa 9F.",
  "",
  "## Integração",
  "",
  "Não foi realizada imputação de valores ausentes, preenchimento entre estações ou construção de séries híbridas. As séries de sensibilidade são mantidas independentes das séries principais.",
  "",
  "## Escopo",
  "",
  "A base integrada desta etapa corresponde aos station-years utilizados como referências anuais válidas de MP10 e que possuem pareamento meteorológico fechado. SUAPE 2022 permanece fora da base integrada até a obtenção de coordenada histórica documentalmente válida da estação PE06.",
  "",
  "## Uso analítico",
  "",
  "A base diária principal integra MP10, variáveis meteorológicas do INMET e precipitação CEMADEN para as análises subsequentes. As limitações de distância, cobertura temporal e disponibilidade por variável devem ser consideradas na interpretação das associações, especialmente para vento em Gaibu 2019, para a interrupção da A301 em 2021 e para umidade relativa em IPOJUCA 2025."
)

writeLines(
  linhas_md,
  file.path(
    SAIDA,
    "resumo_metodologico_etapa9.md"
  ),
  useBytes = TRUE
)

# ------------------------------------------------------------
# 15. RESUMO EXECUCAO
# ------------------------------------------------------------

resumo_exec <- data.table(
  indicador = c(
    "n_station_years_referencia_total",
    "n_station_years_integrados",
    "n_station_years_pendentes",
    "n_linhas_base_diaria_principal",
    "n_grades_diarias_integradas_ok",
    "n_pareamentos_inmet_principal",
    "n_pareamentos_precip_principal",
    "n_valores_imputados",
    "n_series_hibridas"
  ),
  valor = c(
    nrow(pare_consolidado),
    uniqueN(
      base_final[
        ,
        paste(
          estacao_historica,
          ano_alvo
        )
      ]
    ),
    sum(
      is.na(
        pare_consolidado$cod_inmet_principal
      ) |
        is.na(
          pare_consolidado$cod_precip_principal
        )
    ),
    nrow(base_final),
    sum(
      aud_integracao$integracao_ok
    ),
    sum(
      !is.na(
        pare_consolidado$cod_inmet_principal
      )
    ),
    sum(
      !is.na(
        pare_consolidado$cod_precip_principal
      )
    ),
    0L,
    0L
  )
)

fwrite(
  resumo_exec,
  file.path(
    SAIDA,
    "resumo_execucao_consolidacao_final.csv"
  ),
  bom = TRUE
)

# ------------------------------------------------------------
# 16. CONSOLE
# ------------------------------------------------------------

cat("\n============================================================\n")
cat("ETAPA 9O - CONSOLIDACAO METEOROLOGICA FINAL CONCLUIDA\n")
cat("============================================================\n\n")

print(resumo_exec)

cat("\nCobertura das series principais sobre dias validos de MP10:\n")

print(
  cobertura[
    ,
    .(
      estacao_historica,
      ano_alvo,
      n_dias_mp10_validos,
      pct_temp,
      pct_ur,
      pct_vel,
      pct_dir,
      pct_press,
      pct_vetor,
      pct_precip_principal
    )
  ]
)

cat("\nPareamentos consolidados:\n")

print(
  pare_consolidado[
    ,
    .(
      estacao_historica,
      ano_alvo,
      cod_inmet_principal,
      nome_inmet_principal,
      cod_precip_principal,
      nome_precip_principal,
      status_inmet,
      status_precipitacao
    )
  ]
)

cat(
  "\nSTATUS DA ETAPA 9:\n",
  "- base diaria integrada concluida para cinco station-years de referencia;\n",
  "- INMET exclusivo para temperatura, umidade, vento e pressao;\n",
  "- CEMADEN mantido para precipitacao;\n",
  "- sem imputacao e sem series hibridas;\n",
  "- SUAPE 2022 permanece como unica pendencia de pareamento espacial;\n",
  "- base pronta para caracterizacao temporal e analises meteorologicas preliminares.\n",
  sep = ""
)
