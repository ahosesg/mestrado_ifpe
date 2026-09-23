# ============================================================
# ETAPA 9M - CONSOLIDACAO DA BASE HORARIA METEOROLOGICA INMET
# Projeto de Mestrado - MP10 / Suape
# ============================================================
#
# OBJETIVO
# Construir a base horaria meteorologica pareada aos station-years
# de MP10 com pareamento congelado na Etapa 9L.
#
# REGRAS
# - INMET e a unica fonte de temperatura, umidade, vento e pressao;
# - principal e sensibilidade permanecem separadas;
# - nenhuma serie de sensibilidade preenche lacunas da principal;
# - nao ha interpolacao nem imputacao;
# - valores fisicamente invalidos, se existirem, tornam-se NA na
#   coluna analitica, mas o valor bruto e preservado;
# - direcao do vento bruta e preservada e uma versao normalizada
#   em [0, 360) e criada;
# - componentes u/v sao derivados apenas quando velocidade e
#   direcao sao ambas validas;
# - SUAPE 2022 permanece fora da base ate resolver o pareamento.
#
# IMPORTANTE
# Esta etapa consolida os station-years de referencia anual.
# Nao amplia automaticamente o pareamento meteorologico para todos
# os station-years incompletos de MP10.
# ============================================================

if (!requireNamespace("data.table", quietly = TRUE)) {
  stop("Instale o pacote 'data.table' antes de executar.")
}
library(data.table)

# ------------------------------------------------------------
# 1. CAMINHOS
# ------------------------------------------------------------

ARQ_PAREAMENTOS <- file.path(
  "outputs", "09_meteorologia", "multivariaveis_inmet",
  "pareamentos_finais",
  "pareamentos_inmet_finais.csv"
)

ARQ_BASE_9K <- file.path(
  "outputs", "09_meteorologia", "multivariaveis_inmet",
  "qaqc_completude_candidatas",
  "base_horaria_inmet_candidatas_qaqc.rds"
)

ARQ_COMPLETUDE_9K <- file.path(
  "outputs", "09_meteorologia", "multivariaveis_inmet",
  "qaqc_completude_candidatas",
  "completude_horaria_efetiva_candidatas.csv"
)

SAIDA <- file.path(
  "outputs", "09_meteorologia", "multivariaveis_inmet",
  "base_horaria_final"
)

ARQ_RDS_FINAL <- file.path(
  SAIDA,
  "base_horaria_inmet_pareada.rds"
)

dir.create(
  SAIDA,
  recursive = TRUE,
  showWarnings = FALSE
)

for (f in c(
  ARQ_PAREAMENTOS,
  ARQ_BASE_9K,
  ARQ_COMPLETUDE_9K
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

eh_bissexto <- function(ano) {
  (
    ano %% 400 == 0
  ) | (
    ano %% 4 == 0 &
      ano %% 100 != 0
  )
}

horas_esperadas_ano <- function(ano) {
  ifelse(
    eh_bissexto(ano),
    8784L,
    8760L
  )
}

# ------------------------------------------------------------
# 3. LER INSUMOS
# ------------------------------------------------------------

cat("\nCarregando pareamentos da Etapa 9L...\n")

pareamentos <- fread(
  ARQ_PAREAMENTOS,
  encoding = "UTF-8"
)

base_9k <- as.data.table(
  readRDS(
    ARQ_BASE_9K
  )
)

comp_9k <- fread(
  ARQ_COMPLETUDE_9K,
  encoding = "UTF-8"
)

pareamentos[
  ,
  `:=`(
    estacao_historica = as.character(
      estacao_historica
    ),
    ano_alvo = as.integer(
      ano_alvo
    ),
    cod_principal = trimws(
      as.character(
        cod_principal
      )
    ),
    cod_sensibilidade = trimws(
      as.character(
        cod_sensibilidade
      )
    )
  )
]

# Campos vazios vindos do CSV devem ser tratados como ausencia de
# pareamento. Sem esta normalizacao, "" passa no teste !is.na()
# e cria chaves artificiais como "::2022".
pareamentos[
  cod_principal == "",
  cod_principal := NA_character_
]

pareamentos[
  cod_sensibilidade == "",
  cod_sensibilidade := NA_character_
]

base_9k[
  ,
  `:=`(
    cod_estacao = as.character(
      cod_estacao
    ),
    ano_local = as.integer(
      ano_local
    )
  )
]

comp_9k[
  ,
  `:=`(
    cod_estacao = as.character(
      cod_estacao
    ),
    ano_local = as.integer(
      ano_local
    )
  )
]

# ------------------------------------------------------------
# 4. MAPA LONGO PRINCIPAL + SENSIBILIDADE
# ------------------------------------------------------------

cat("Construindo mapa de series principal e sensibilidade...\n")

mapa_principal <- pareamentos[
  !is.na(cod_principal) &
    nzchar(cod_principal),
  .(
    estacao_historica,
    ano_alvo,
    papel_serie = "principal",
    cod_estacao = cod_principal,
    nome_estacao_pareada = nome_principal,
    distancia_km = distancia_principal_km,
    status_pareamento,
    justificativa
  )
]

mapa_sens <- pareamentos[
  !is.na(cod_sensibilidade) &
    nzchar(cod_sensibilidade),
  .(
    estacao_historica,
    ano_alvo,
    papel_serie = "sensibilidade",
    cod_estacao = cod_sensibilidade,
    nome_estacao_pareada = nome_sensibilidade,
    distancia_km = distancia_sensibilidade_km,
    status_pareamento,
    justificativa
  )
]

mapa <- rbindlist(
  list(
    mapa_principal,
    mapa_sens
  ),
  fill = TRUE
)

setorder(
  mapa,
  ano_alvo,
  estacao_historica,
  papel_serie
)

if (anyDuplicated(
  mapa[
    ,
    .(
      estacao_historica,
      ano_alvo,
      papel_serie
    )
  ]
)) {
  stop(
    "Ha mais de uma estacao atribuida ao mesmo alvo/papel."
  )
}

fwrite(
  mapa,
  file.path(
    SAIDA,
    "mapeamento_series_horarias.csv"
  ),
  bom = TRUE
)

# ------------------------------------------------------------
# 5. PENDENCIAS
# ------------------------------------------------------------

pendencias <- pareamentos[
  is.na(cod_principal),
  .(
    estacao_historica,
    ano_alvo,
    status_pareamento,
    justificativa
  )
]

fwrite(
  pendencias,
  file.path(
    SAIDA,
    "pendencias_base_horaria.csv"
  ),
  bom = TRUE
)

# ------------------------------------------------------------
# 6. VERIFICAR DISPONIBILIDADE NO RDS DA 9K
# ------------------------------------------------------------

# Normaliza as chaves antes da auditoria. A verificacao e feita por
# chave composta explicita para evitar ambiguidades de merge e para
# informar exatamente qual serie esta ausente, caso exista.
base_9k[
  ,
  `:=`(
    cod_estacao = trimws(
      as.character(cod_estacao)
    ),
    ano_local = as.integer(
      ano_local
    )
  )
]

mapa[
  ,
  `:=`(
    cod_estacao = trimws(
      as.character(cod_estacao)
    ),
    ano_alvo = as.integer(
      ano_alvo
    )
  )
]

chaves_disponiveis <- unique(
  base_9k[
    !is.na(cod_estacao) &
      !is.na(ano_local),
    .(
      cod_estacao,
      ano_local
    )
  ]
)

chaves_disponiveis[
  ,
  chave := paste0(
    cod_estacao,
    "::",
    ano_local
  )
]

aud_chaves <- mapa[
  ,
  .(
    estacao_historica,
    ano_alvo,
    papel_serie,
    cod_estacao
  )
]

aud_chaves[
  ,
  chave := paste0(
    cod_estacao,
    "::",
    ano_alvo
  )
]

aud_chaves[
  ,
  disponivel_9k := chave %chin%
    chaves_disponiveis$chave
]

fwrite(
  aud_chaves,
  file.path(
    SAIDA,
    "auditoria_chaves_pareadas.csv"
  ),
  bom = TRUE
)

faltantes_9k <- aud_chaves[
  disponivel_9k == FALSE
]

if (nrow(faltantes_9k)) {

  cat(
    "\nSeries pareadas nao localizadas no RDS da 9K:\n"
  )

  print(
    faltantes_9k[
      ,
      .(
        estacao_historica,
        ano_alvo,
        papel_serie,
        cod_estacao,
        chave
      )
    ]
  )

  cat(
    "\nChaves estacao-ano disponiveis no RDS da 9K:\n"
  )

  print(
    chaves_disponiveis[
      order(
        ano_local,
        cod_estacao
      )
    ]
  )

  stop(
    "Uma ou mais series pareadas nao estao disponiveis na base da 9K. ",
    "Consulte auditoria_chaves_pareadas.csv e a listagem acima."
  )
}

# ------------------------------------------------------------
# 7. PAREAR A BASE HORARIA
# ------------------------------------------------------------

cat("Pareando base horaria aos station-years de MP10...\n")

# Evita colisao de nomes no merge.
base_trabalho <- copy(base_9k)

setnames(
  base_trabalho,
  "nome_estacao",
  "nome_estacao_inmet"
)

base_final <- merge(
  mapa,
  base_trabalho,
  by.x = c(
    "cod_estacao",
    "ano_alvo"
  ),
  by.y = c(
    "cod_estacao",
    "ano_local"
  ),
  all.x = TRUE,
  allow.cartesian = TRUE
)

# `all.x = TRUE` produziria uma linha artificial com NA caso alguma
# chave nao tivesse registros. Isso nao deve ocorrer depois da auditoria.
linhas_sem_hora <- base_final[
  is.na(datahora_local)
]

if (nrow(linhas_sem_hora)) {
  print(
    linhas_sem_hora[
      ,
      .(
        estacao_historica,
        ano_alvo,
        papel_serie,
        cod_estacao
      )
    ]
  )

  stop(
    "Ha pareamento fechado sem data horaria correspondente na base da 9K."
  )
}

# ------------------------------------------------------------
# 8. COLUNAS ANALITICAS
# ------------------------------------------------------------

cat("Criando colunas analiticas sem imputacao...\n")

base_final[
  ,
  temperatura_ar_c_analitica := fifelse(
    temperatura_valida %in% TRUE,
    temperatura_ar_c,
    NA_real_
  )
]

base_final[
  ,
  umidade_relativa_pct_analitica := fifelse(
    umidade_valida %in% TRUE,
    umidade_relativa_pct,
    NA_real_
  )
]

base_final[
  ,
  velocidade_vento_ms_analitica := fifelse(
    velocidade_vento_valida %in% TRUE,
    velocidade_vento_ms,
    NA_real_
  )
]

base_final[
  ,
  direcao_vento_graus_analitica := fifelse(
    direcao_vento_valida %in% TRUE,
    direcao_vento_graus %% 360,
    NA_real_
  )
]

base_final[
  ,
  pressao_estacao_hpa_analitica := fifelse(
    pressao_valida %in% TRUE,
    pressao_estacao_hpa,
    NA_real_
  )
]

base_final[
  ,
  vento_par_valido := (
    velocidade_vento_valida %in% TRUE &
      direcao_vento_valida %in% TRUE
  )
]

# Convencao meteorologica:
# direcao indica DE ONDE o vento sopra.
# u < 0 = componente para oeste; u > 0 = componente para leste.
# v < 0 = componente para sul;  v > 0 = componente para norte.
base_final[
  ,
  vento_u_ms := fifelse(
    vento_par_valido,
    -velocidade_vento_ms_analitica *
      sin(
        direcao_vento_graus_analitica *
          pi / 180
      ),
    NA_real_
  )
]

base_final[
  ,
  vento_v_ms := fifelse(
    vento_par_valido,
    -velocidade_vento_ms_analitica *
      cos(
        direcao_vento_graus_analitica *
          pi / 180
      ),
    NA_real_
  )
]

base_final[
  ,
  fonte_meteorologica := "INMET"
]

base_final[
  ,
  regra_imputacao := "sem_imputacao"
]

# ------------------------------------------------------------
# 9. ORDENAR E SALVAR RDS
# ------------------------------------------------------------

setorder(
  base_final,
  estacao_historica,
  ano_alvo,
  papel_serie,
  datahora_local
)

saveRDS(
  base_final,
  ARQ_RDS_FINAL,
  compress = "xz"
)

# ------------------------------------------------------------
# 10. RESUMO DE COMPLETUDE DA BASE FINAL
# ------------------------------------------------------------

cat("Auditando completude da base horaria consolidada...\n")

resumo <- base_final[
  ,
  {
    n_esperado <- horas_esperadas_ano(
      unique(ano_alvo)
    )

    list(
      n_horas_esperadas = n_esperado,
      n_registros_horarios = .N,
      n_timestamps_unicos = uniqueN(
        datahora_local
      ),
      primeira_datahora = min(
        datahora_local,
        na.rm = TRUE
      ),
      ultima_datahora = max(
        datahora_local,
        na.rm = TRUE
      ),
      n_temp_validas = sum(
        !is.na(
          temperatura_ar_c_analitica
        )
      ),
      n_ur_validas = sum(
        !is.na(
          umidade_relativa_pct_analitica
        )
      ),
      n_vel_validas = sum(
        !is.na(
          velocidade_vento_ms_analitica
        )
      ),
      n_dir_validas = sum(
        !is.na(
          direcao_vento_graus_analitica
        )
      ),
      n_press_validas = sum(
        !is.na(
          pressao_estacao_hpa_analitica
        )
      ),
      n_pares_vento_validos = sum(
        vento_par_valido %in% TRUE
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
    status_pareamento
  )
]

resumo[
  ,
  `:=`(
    pct_timestamps =
      100 *
        n_timestamps_unicos /
        n_horas_esperadas,
    pct_temp =
      100 *
        n_temp_validas /
        n_horas_esperadas,
    pct_ur =
      100 *
        n_ur_validas /
        n_horas_esperadas,
    pct_vel =
      100 *
        n_vel_validas /
        n_horas_esperadas,
    pct_dir =
      100 *
        n_dir_validas /
        n_horas_esperadas,
    pct_press =
      100 *
        n_press_validas /
        n_horas_esperadas,
    pct_pares_vento =
      100 *
        n_pares_vento_validos /
        n_horas_esperadas
  )
]

resumo[
  ,
  completude_min_nuclear_pct := pmin(
    pct_temp,
    pct_ur,
    pct_vel,
    pct_dir,
    na.rm = FALSE
  )
]

fwrite(
  resumo,
  file.path(
    SAIDA,
    "resumo_base_horaria_pareada.csv"
  ),
  bom = TRUE
)

# ------------------------------------------------------------
# 11. AUDITORIA CONTRA A ETAPA 9K
# ------------------------------------------------------------

aud_9k <- merge(
  resumo[
    ,
    .(
      estacao_historica,
      ano_alvo,
      papel_serie,
      cod_estacao,
      pct_timestamps_9m = pct_timestamps,
      pct_temp_9m = pct_temp,
      pct_ur_9m = pct_ur,
      pct_vel_9m = pct_vel,
      pct_dir_9m = pct_dir,
      pct_press_9m = pct_press,
      completude_min_nuclear_9m =
        completude_min_nuclear_pct
    )
  ],
  comp_9k[
    ,
    .(
      cod_estacao,
      ano_alvo = ano_local,
      pct_timestamps_9k =
        pct_timestamps_presentes,
      pct_temp_9k =
        pct_temp_efetiva,
      pct_ur_9k =
        pct_ur_efetiva,
      pct_vel_9k =
        pct_vel_efetiva,
      pct_dir_9k =
        pct_dir_efetiva,
      pct_press_9k =
        pct_press_efetiva,
      completude_min_nuclear_9k =
        completude_min_nuclear_efetiva
    )
  ],
  by = c(
    "cod_estacao",
    "ano_alvo"
  ),
  all.x = TRUE
)

aud_9k[
  ,
  `:=`(
    dif_timestamps =
      abs(
        pct_timestamps_9m -
          pct_timestamps_9k
      ),
    dif_temp =
      abs(
        pct_temp_9m -
          pct_temp_9k
      ),
    dif_ur =
      abs(
        pct_ur_9m -
          pct_ur_9k
      ),
    dif_vel =
      abs(
        pct_vel_9m -
          pct_vel_9k
      ),
    dif_dir =
      abs(
        pct_dir_9m -
          pct_dir_9k
      ),
    dif_press =
      abs(
        pct_press_9m -
          pct_press_9k
      ),
    dif_min_nuclear =
      abs(
        completude_min_nuclear_9m -
          completude_min_nuclear_9k
      )
  )
]

aud_9k[
  ,
  max_diferenca_pct := pmax(
    dif_timestamps,
    dif_temp,
    dif_ur,
    dif_vel,
    dif_dir,
    dif_press,
    dif_min_nuclear,
    na.rm = TRUE
  )
]

aud_9k[
  ,
  auditoria_ok := (
    !is.na(max_diferenca_pct) &
      max_diferenca_pct < 1e-8
  )
]

fwrite(
  aud_9k,
  file.path(
    SAIDA,
    "auditoria_reproducao_9k.csv"
  ),
  bom = TRUE
)

if (any(!aud_9k$auditoria_ok)) {
  stop(
    "A base consolidada nao reproduziu exatamente a completude da 9K."
  )
}

# ------------------------------------------------------------
# 12. RESUMO EXECUCAO
# ------------------------------------------------------------

resumo_exec <- data.table(
  indicador = c(
    "n_alvos_com_base_horaria",
    "n_alvos_pendentes",
    "n_series_principais",
    "n_series_sensibilidade",
    "n_linhas_base_pareada",
    "n_estacoes_inmet_distintas",
    "n_auditorias_9k_ok",
    "n_series_hibridas",
    "n_valores_imputados"
  ),
  valor = c(
    uniqueN(
      mapa[
        papel_serie == "principal",
        paste(
          estacao_historica,
          ano_alvo
        )
      ]
    ),
    nrow(pendencias),
    sum(
      mapa$papel_serie ==
        "principal"
    ),
    sum(
      mapa$papel_serie ==
        "sensibilidade"
    ),
    nrow(base_final),
    uniqueN(
      base_final$cod_estacao
    ),
    sum(
      aud_9k$auditoria_ok
    ),
    0L,
    0L
  )
)

fwrite(
  resumo_exec,
  file.path(
    SAIDA,
    "resumo_execucao_base_horaria_final.csv"
  ),
  bom = TRUE
)

# ------------------------------------------------------------
# 13. CONSOLE
# ------------------------------------------------------------

cat("\n============================================================\n")
cat("ETAPA 9M - BASE HORARIA INMET CONSOLIDADA\n")
cat("============================================================\n\n")

print(resumo_exec)

cat("\nResumo das series principais:\n")

print(
  resumo[
    papel_serie == "principal",
    .(
      estacao_historica,
      ano_alvo,
      cod_estacao,
      nome_estacao_inmet,
      distancia_km,
      n_horas_esperadas,
      n_timestamps_unicos,
      pct_timestamps,
      pct_temp,
      pct_ur,
      pct_vel,
      pct_dir,
      pct_press,
      completude_min_nuclear_pct
    )
  ]
)

cat(
  "\nREGRAS PRESERVADAS:\n",
  "- INMET e a unica fonte das variaveis multivariaveis;\n",
  "- principal e sensibilidade permanecem independentes;\n",
  "- nao houve imputacao nem preenchimento entre estacoes;\n",
  "- direcao do vento bruta foi preservada e a versao analitica foi normalizada para [0,360);\n",
  "- componentes u/v foram derivados apenas de pares velocidade-direcao validos;\n",
  "- SUAPE 2022 permanece pendente e nao integra esta base.\n",
  sep = ""
)
