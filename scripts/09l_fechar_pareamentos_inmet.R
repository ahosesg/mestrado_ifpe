# ============================================================
# ETAPA 9L - FECHAMENTO DOS PAREAMENTOS INMET
# Projeto de Mestrado - MP10 / Suape
# ============================================================
#
# OBJETIVO
# Congelar os pareamentos entre as series de MP10 e as estacoes
# automaticas do INMET, com base na triagem espacial (9J) e na
# completude efetiva / sobreposicao com MP10 (9K).
#
# PRINCIPIOS
# - INMET e a unica fonte para temperatura, umidade, vento e pressao;
# - proximidade espacial tem prioridade, desde que a serie contenha
#   informacao suficiente para analise;
# - maior completude nao justifica escolher automaticamente uma
#   estacao muito mais distante;
# - a estacao de sensibilidade NAO sera usada para preencher lacunas
#   da principal;
# - nenhuma serie hibrida sera criada;
# - SUAPE 2022 permanece pendente de coordenada historica valida.
#
# PAREAMENTOS CONGELADOS NESTA ETAPA
# IFPE 2018      -> principal A301 Recife; sensibilidade A328 Surubim
# Gaibu 2019     -> principal A301 Recife; sensibilidade A328 Surubim
# CUPE 2021      -> principal A301 Recife; sensibilidade A357 Palmares
# IPOJUCA 2021   -> principal A301 Recife; sensibilidade A357 Palmares
# IPOJUCA 2025   -> principal A357 Palmares; sensibilidade A341 Caruaru
# SUAPE 2022     -> pendente
# ============================================================

if (!requireNamespace("data.table", quietly = TRUE)) {
  stop("Instale o pacote 'data.table' antes de executar.")
}
library(data.table)

# ------------------------------------------------------------
# 1. CAMINHOS
# ------------------------------------------------------------

ARQ_SOBREPOSICAO <- file.path(
  "outputs", "09_meteorologia", "multivariaveis_inmet",
  "qaqc_completude_candidatas",
  "sobreposicao_mp10_multivariaveis_candidatas.csv"
)

ARQ_COMPLETUDE <- file.path(
  "outputs", "09_meteorologia", "multivariaveis_inmet",
  "qaqc_completude_candidatas",
  "completude_horaria_efetiva_candidatas.csv"
)

ARQ_AUDITORIA <- file.path(
  "outputs", "09_meteorologia", "multivariaveis_inmet",
  "qaqc_completude_candidatas",
  "auditoria_denominador_mp10.csv"
)

SAIDA <- file.path(
  "outputs", "09_meteorologia", "multivariaveis_inmet",
  "pareamentos_finais"
)

dir.create(SAIDA, recursive = TRUE, showWarnings = FALSE)

for (f in c(
  ARQ_SOBREPOSICAO,
  ARQ_COMPLETUDE,
  ARQ_AUDITORIA
)) {
  if (!file.exists(f)) {
    stop("Arquivo necessario nao encontrado: ", f)
  }
}

# ------------------------------------------------------------
# 2. LER RESULTADOS DAS ETAPAS ANTERIORES
# ------------------------------------------------------------

cat("\nCarregando resultados das Etapas 9J e 9K...\n")

sob <- fread(
  ARQ_SOBREPOSICAO,
  encoding = "UTF-8"
)

comp <- fread(
  ARQ_COMPLETUDE,
  encoding = "UTF-8"
)

aud <- fread(
  ARQ_AUDITORIA,
  encoding = "UTF-8"
)

sob[
  ,
  `:=`(
    cod_estacao = as.character(cod_estacao),
    ano_alvo = as.integer(ano_alvo),
    estacao_historica = as.character(estacao_historica)
  )
]

comp[
  ,
  `:=`(
    cod_estacao = as.character(cod_estacao),
    ano_local = as.integer(ano_local)
  )
]

# ------------------------------------------------------------
# 3. VALIDAR DENOMINADORES MP10
# ------------------------------------------------------------

if (any(aud$n_denominadores != 1L)) {
  stop(
    "A auditoria do denominador MP10 nao esta consistente."
  )
}

# ------------------------------------------------------------
# 4. DEFINIR PAREAMENTOS
# ------------------------------------------------------------

pareamentos <- data.table(
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
  cod_principal = c(
    "A301",
    "A301",
    "A301",
    "A301",
    "A357",
    NA_character_
  ),
  cod_sensibilidade = c(
    "A328",
    "A328",
    "A357",
    "A357",
    "A341",
    NA_character_
  ),
  status_pareamento = c(
    "fechado",
    "fechado_com_limitacao_vento",
    "fechado_com_limitacao_temporal_2021",
    "fechado_com_limitacao_temporal_2021",
    "fechado_com_limitacao_espacial",
    "pendente_coordenada_historica"
  ),
  justificativa = c(
    paste(
      "A301 e a automatica INMET mais proxima entre as candidatas",
      "avaliadas e apresenta elevada completude efetiva em 2018."
    ),
    paste(
      "A301 e a automatica INMET mais proxima; temperatura, umidade",
      "e pressao apresentam cobertura quase integral, enquanto vento",
      "possui cobertura reduzida e deve ser interpretado com cautela."
    ),
    paste(
      "A301 e espacialmente mais proxima e apresenta melhor equilibrio",
      "entre distancia e disponibilidade que as alternativas regionais;",
      "a serie termina em novembro de 2021 e essa perda temporal deve",
      "ser explicitada."
    ),
    paste(
      "A301 e espacialmente mais proxima e apresenta melhor equilibrio",
      "entre distancia e disponibilidade que as alternativas regionais;",
      "a serie termina em novembro de 2021 e essa perda temporal deve",
      "ser explicitada."
    ),
    paste(
      "A357 e a automatica INMET mais proxima disponivel em 2025;",
      "a distancia e elevada e a umidade relativa possui menor",
      "completude que as demais variaveis."
    ),
    paste(
      "Pareamento nao congelado porque a estacao historica SUAPE/PE06",
      "ainda nao possui coordenada documental valida."
    )
  )
)

# ------------------------------------------------------------
# 5. ANEXAR METRICAS DA PRINCIPAL
# ------------------------------------------------------------

metricas_principal <- merge(
  pareamentos[
    !is.na(cod_principal),
    .(
      estacao_historica,
      ano_alvo,
      cod_estacao = cod_principal
    )
  ],
  sob,
  by = c(
    "estacao_historica",
    "ano_alvo",
    "cod_estacao"
  ),
  all.x = TRUE
)

if (
  nrow(metricas_principal) !=
    sum(!is.na(pareamentos$cod_principal))
) {
  stop(
    "Uma ou mais estacoes principais nao foram localizadas nos resultados da 9K."
  )
}

if (
  any(
    is.na(
      metricas_principal$distancia_km
    )
  )
) {
  stop(
    "Ha pareamento principal sem metricas da Etapa 9K."
  )
}

setnames(
  metricas_principal,
  c(
    "cod_estacao",
    "nome_estacao",
    "distancia_km",
    "n_dias_mp10_validos",
    "pct_overlap_nuclear_16h",
    "pct_overlap_nuclear_18h",
    "pct_overlap_nuclear_20h",
    "completude_min_nuclear_efetiva",
    "pct_temp_efetiva",
    "pct_ur_efetiva",
    "pct_vel_efetiva",
    "pct_dir_efetiva",
    "pct_press_efetiva",
    "pct_timestamps_presentes"
  ),
  c(
    "cod_principal",
    "nome_principal",
    "distancia_principal_km",
    "n_dias_mp10_validos",
    "principal_overlap_nuclear_16h_pct",
    "principal_overlap_nuclear_18h_pct",
    "principal_overlap_nuclear_20h_pct",
    "principal_completude_min_nuclear_pct",
    "principal_temp_pct",
    "principal_ur_pct",
    "principal_vel_vento_pct",
    "principal_dir_vento_pct",
    "principal_pressao_pct",
    "principal_timestamps_presentes_pct"
  )
)

cols_principal <- c(
  "estacao_historica",
  "ano_alvo",
  "cod_principal",
  "nome_principal",
  "distancia_principal_km",
  "n_dias_mp10_validos",
  "principal_overlap_nuclear_16h_pct",
  "principal_overlap_nuclear_18h_pct",
  "principal_overlap_nuclear_20h_pct",
  "principal_completude_min_nuclear_pct",
  "principal_temp_pct",
  "principal_ur_pct",
  "principal_vel_vento_pct",
  "principal_dir_vento_pct",
  "principal_pressao_pct",
  "principal_timestamps_presentes_pct"
)

metricas_principal <- metricas_principal[
  ,
  ..cols_principal
]

# ------------------------------------------------------------
# 6. ANEXAR METRICAS DA SENSIBILIDADE
# ------------------------------------------------------------

metricas_sens <- merge(
  pareamentos[
    !is.na(cod_sensibilidade),
    .(
      estacao_historica,
      ano_alvo,
      cod_estacao = cod_sensibilidade
    )
  ],
  sob,
  by = c(
    "estacao_historica",
    "ano_alvo",
    "cod_estacao"
  ),
  all.x = TRUE
)

if (
  nrow(metricas_sens) !=
    sum(!is.na(pareamentos$cod_sensibilidade))
) {
  stop(
    "Uma ou mais estacoes de sensibilidade nao foram localizadas na 9K."
  )
}

if (
  any(
    is.na(
      metricas_sens$distancia_km
    )
  )
) {
  stop(
    "Ha pareamento de sensibilidade sem metricas da Etapa 9K."
  )
}

setnames(
  metricas_sens,
  c(
    "cod_estacao",
    "nome_estacao",
    "distancia_km",
    "pct_overlap_nuclear_16h",
    "pct_overlap_nuclear_18h",
    "pct_overlap_nuclear_20h",
    "completude_min_nuclear_efetiva",
    "pct_temp_efetiva",
    "pct_ur_efetiva",
    "pct_vel_efetiva",
    "pct_dir_efetiva",
    "pct_press_efetiva",
    "pct_timestamps_presentes"
  ),
  c(
    "cod_sensibilidade",
    "nome_sensibilidade",
    "distancia_sensibilidade_km",
    "sens_overlap_nuclear_16h_pct",
    "sens_overlap_nuclear_18h_pct",
    "sens_overlap_nuclear_20h_pct",
    "sens_completude_min_nuclear_pct",
    "sens_temp_pct",
    "sens_ur_pct",
    "sens_vel_vento_pct",
    "sens_dir_vento_pct",
    "sens_pressao_pct",
    "sens_timestamps_presentes_pct"
  )
)

cols_sens <- c(
  "estacao_historica",
  "ano_alvo",
  "cod_sensibilidade",
  "nome_sensibilidade",
  "distancia_sensibilidade_km",
  "sens_overlap_nuclear_16h_pct",
  "sens_overlap_nuclear_18h_pct",
  "sens_overlap_nuclear_20h_pct",
  "sens_completude_min_nuclear_pct",
  "sens_temp_pct",
  "sens_ur_pct",
  "sens_vel_vento_pct",
  "sens_dir_vento_pct",
  "sens_pressao_pct",
  "sens_timestamps_presentes_pct"
)

metricas_sens <- metricas_sens[
  ,
  ..cols_sens
]

# ------------------------------------------------------------
# 7. CONSOLIDAR
# ------------------------------------------------------------

final <- merge(
  pareamentos,
  metricas_principal,
  by = c(
    "estacao_historica",
    "ano_alvo",
    "cod_principal"
  ),
  all.x = TRUE
)

final <- merge(
  final,
  metricas_sens,
  by = c(
    "estacao_historica",
    "ano_alvo",
    "cod_sensibilidade"
  ),
  all.x = TRUE
)

# ------------------------------------------------------------
# 8. REGRAS DE USO DAS SERIES
# ------------------------------------------------------------

final[
  ,
  uso_principal := fifelse(
    status_pareamento == "pendente_coordenada_historica",
    "nao_utilizar_ate_resolver_pareamento",
    paste(
      "usar a estacao principal para temperatura, umidade,",
      "velocidade/direcao do vento e pressao, respeitando",
      "a disponibilidade propria de cada variavel"
    )
  )
]

final[
  ,
  uso_sensibilidade := fifelse(
    is.na(cod_sensibilidade),
    NA_character_,
    paste(
      "usar apenas em analise de sensibilidade; nao preencher",
      "lacunas nem concatenar com a serie principal"
    )
  )
]

final[
  ,
  regra_vento := fifelse(
    status_pareamento == "pendente_coordenada_historica",
    NA_character_,
    paste(
      "direcao deve ser tratada como circular ou por componentes",
      "vetoriais; nao calcular media aritmetica simples de graus"
    )
  )
]

final[
  ,
  regra_lacunas := fifelse(
    status_pareamento == "pendente_coordenada_historica",
    NA_character_,
    paste(
      "ausencias permanecem ausentes; nao imputar com outra estacao",
      "e nao completar a principal com a serie de sensibilidade"
    )
  )
]

setorder(
  final,
  ano_alvo,
  estacao_historica
)

# ------------------------------------------------------------
# 9. TABELA COMPACTA PARA METODOLOGIA
# ------------------------------------------------------------

metodologia <- final[
  ,
  .(
    estacao_mp10 = estacao_historica,
    ano = ano_alvo,
    estacao_inmet_principal = fifelse(
      is.na(cod_principal),
      NA_character_,
      paste0(
        cod_principal,
        " - ",
        nome_principal
      )
    ),
    distancia_principal_km,
    completude_nuclear_principal_pct =
      principal_completude_min_nuclear_pct,
    sobreposicao_16h_mp10_pct =
      principal_overlap_nuclear_16h_pct,
    estacao_inmet_sensibilidade = fifelse(
      is.na(cod_sensibilidade),
      NA_character_,
      paste0(
        cod_sensibilidade,
        " - ",
        nome_sensibilidade
      )
    ),
    distancia_sensibilidade_km,
    status_pareamento,
    justificativa
  )
]

# ------------------------------------------------------------
# 10. LIMITACOES
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
  limitacao_principal = c(
    paste(
      "Estacao INMET a aproximadamente 37 km; interpretar como",
      "condicao meteorologica regional, nao micrometeorologia local."
    ),
    paste(
      "Velocidade e direcao do vento possuem apenas cerca de 58%",
      "de completude horaria efetiva na A301 em 2019."
    ),
    paste(
      "A301 encerra registros em novembro de 2021; alem disso,",
      "a localizacao CUPE usa EDCUPE 2024 como proxy espacial."
    ),
    paste(
      "A301 encerra registros em novembro de 2021 e esta a cerca",
      "de 43 km do alvo espacial adotado."
    ),
    paste(
      "A357 esta a cerca de 66 km; umidade relativa possui",
      "aproximadamente 76% de completude efetiva."
    ),
    paste(
      "Coordenada historica de SUAPE/PE06 ainda nao foi",
      "documentalmente resolvida."
    )
  )
)

# ------------------------------------------------------------
# 11. EXPORTAR
# ------------------------------------------------------------

fwrite(
  final,
  file.path(
    SAIDA,
    "pareamentos_inmet_finais.csv"
  ),
  bom = TRUE
)

fwrite(
  metodologia,
  file.path(
    SAIDA,
    "pareamentos_inmet_metodologia.csv"
  ),
  bom = TRUE
)

fwrite(
  limitacoes,
  file.path(
    SAIDA,
    "limitacoes_pareamentos_inmet.csv"
  ),
  bom = TRUE
)

resumo_exec <- data.table(
  indicador = c(
    "n_alvos_total",
    "n_pareamentos_fechados",
    "n_pareamentos_pendentes",
    "n_principais_A301",
    "n_principais_A357",
    "n_series_sensibilidade",
    "n_series_hibridas_permitidas"
  ),
  valor = c(
    nrow(final),
    sum(
      final$status_pareamento !=
        "pendente_coordenada_historica"
    ),
    sum(
      final$status_pareamento ==
        "pendente_coordenada_historica"
    ),
    sum(
      final$cod_principal == "A301",
      na.rm = TRUE
    ),
    sum(
      final$cod_principal == "A357",
      na.rm = TRUE
    ),
    sum(
      !is.na(
        final$cod_sensibilidade
      )
    ),
    0L
  )
)

fwrite(
  resumo_exec,
  file.path(
    SAIDA,
    "resumo_execucao_pareamentos_inmet.csv"
  ),
  bom = TRUE
)

# ------------------------------------------------------------
# 12. CONSOLE
# ------------------------------------------------------------

cat("\n============================================================\n")
cat("ETAPA 9L - PAREAMENTOS INMET CONGELADOS\n")
cat("============================================================\n\n")

print(resumo_exec)

cat("\nPareamentos principais:\n")

print(
  final[
    ,
    .(
      estacao_historica,
      ano_alvo,
      cod_principal,
      nome_principal,
      distancia_principal_km,
      principal_completude_min_nuclear_pct,
      principal_overlap_nuclear_16h_pct,
      cod_sensibilidade,
      nome_sensibilidade,
      status_pareamento
    )
  ]
)

cat(
  "\nREGRAS CONGELADAS:\n",
  "- INMET e a unica fonte para temperatura, umidade, vento e pressao;\n",
  "- sensibilidade nao preenche lacunas da principal;\n",
  "- nao serao criadas series hibridas entre estacoes;\n",
  "- cada variavel podera usar todos os seus horarios/dias validos;\n",
  "- direcao do vento sera tratada como variavel circular;\n",
  "- SUAPE 2022 permanece pendente de coordenada historica valida.\n",
  sep = ""
)
