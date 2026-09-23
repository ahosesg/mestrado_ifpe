# ============================================================
# ETAPA 9J - TRIAGEM ESPACIAL E TEMPORAL MULTIVARIAVEL INMET
# Projeto de Mestrado - MP10 / Suape
# ============================================================
#
# OBJETIVO
# Cruzar a disponibilidade multivariavel inventariada na Etapa 9I
# com os alvos de MP10 ja espacialmente resolvidos:
# - IFPE 2018
# - Gaibu 2019
# - IPOJUCA 2021
# - CUPE 2021 (coordenada EDCUPE 2024 como proxy espacial)
# - IPOJUCA 2025
#
# SUAPE 2022 permanece fora da triagem espacial por ausencia de
# coordenada historica documentalmente valida.
#
# PRINCIPIOS
# - estacoes automaticas sao candidatas principais;
# - estacoes convencionais ficam como diagnostico/secundarias;
# - nao ha escore ponderado;
# - proximidade e completude sao mantidas separadas;
# - as variaveis nucleares sao:
#   temperatura, umidade, velocidade e direcao do vento;
# - pressao e complementar;
# - direcao do vento sera tratada como variavel circular depois;
# - nenhuma estacao e escolhida automaticamente nesta etapa.
# ============================================================

if (!requireNamespace("data.table", quietly = TRUE)) {
  stop("Instale o pacote 'data.table' antes de executar.")
}
library(data.table)

# ------------------------------------------------------------
# 1. CAMINHOS
# ------------------------------------------------------------

ARQ_DISP <- file.path(
  "outputs", "09_meteorologia", "multivariaveis_inmet",
  "disponibilidade_estacao_ano_variavel.csv"
)

SAIDA <- file.path(
  "outputs", "09_meteorologia", "multivariaveis_inmet",
  "triagem_espacial_temporal"
)

dir.create(SAIDA, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(ARQ_DISP)) {
  stop("Arquivo nao encontrado: ", ARQ_DISP)
}

# ------------------------------------------------------------
# 2. ALVOS MP10
# ------------------------------------------------------------

# Coordenadas adotadas:
# Gaibu 2019: coordenada historica documentada.
# IFPE/IPOJUCA: coordenadas MonitorAr 2024, usadas como referencia
# espacial provisoria para as series historicas.
# CUPE 2021: EDCUPE 2024 como proxy espacial, explicitamente
# provisoria ate confirmacao documental.

alvos <- data.table(
  estacao_historica = c(
    "IFPE",
    "Gaibu",
    "IPOJUCA",
    "CUPE",
    "IPOJUCA"
  ),
  ano_alvo = c(
    2018L,
    2019L,
    2021L,
    2021L,
    2025L
  ),
  latitude_mp10 = c(
    -8.38456,
    -8.328991667,
    -8.44359,
    -8.39754,
    -8.44359
  ),
  longitude_mp10 = c(
    -35.04230,
    -34.956294444,
    -35.01403,
    -35.04313,
    -35.01403
  ),
  origem_coordenada = c(
    "MonitorAr 2024, proxy espacial da serie historica",
    "Coordenada historica documentada PE01",
    "MonitorAr 2024, proxy espacial da serie historica",
    "EDCUPE 2024, proxy espacial provisoria para CUPE",
    "MonitorAr 2024, proxy espacial da serie historica"
  ),
  status_espacial = c(
    "provisorio",
    "documentado",
    "provisorio",
    "proxy_CUPE",
    "provisorio"
  )
)

fwrite(
  alvos,
  file.path(
    SAIDA,
    "alvos_mp10_triagem_multivariavel.csv"
  ),
  bom = TRUE
)

pendencia_suape <- data.table(
  estacao_historica = "SUAPE",
  ano_alvo = 2022L,
  status = "pendente_coordenada_historica_valida",
  observacao = paste(
    "Nao realizar pareamento espacial automatico ate obter",
    "coordenada historica documentalmente valida da estacao PE06."
  )
)

fwrite(
  pendencia_suape,
  file.path(
    SAIDA,
    "pendencia_suape_2022.csv"
  ),
  bom = TRUE
)

# ------------------------------------------------------------
# 3. FUNCOES
# ------------------------------------------------------------

haversine_km <- function(lat1, lon1, lat2, lon2) {
  r <- 6371.0088
  rad <- pi / 180

  p1 <- lat1 * rad
  p2 <- lat2 * rad
  dp <- (lat2 - lat1) * rad
  dl <- (lon2 - lon1) * rad

  a <- sin(dp / 2)^2 +
    cos(p1) * cos(p2) * sin(dl / 2)^2

  2 * r * atan2(
    sqrt(a),
    sqrt(1 - a)
  )
}

primeiro_nao_na <- function(x) {
  z <- unique(na.omit(x))
  if (!length(z)) return(NA)
  z[1]
}

# ------------------------------------------------------------
# 4. LER DISPONIBILIDADE
# ------------------------------------------------------------

cat("\nCarregando disponibilidade da Etapa 9I...\n")

disp <- fread(
  ARQ_DISP,
  encoding = "UTF-8"
)

disp[
  ,
  `:=`(
    cod_estacao = as.character(cod_estacao),
    nome_estacao = as.character(nome_estacao),
    tipo_estacao = as.character(tipo_estacao),
    ano = as.integer(ano),
    variavel = as.character(variavel),
    latitude = as.numeric(latitude),
    longitude = as.numeric(longitude),
    pct_validos = as.numeric(pct_validos),
    n_datas_com_valor = as.integer(n_datas_com_valor),
    n_meses_com_valor = as.integer(n_meses_com_valor)
  )
]

# ------------------------------------------------------------
# 5. MATRIZ ESTACAO-ANO
# ------------------------------------------------------------

cat("Construindo matriz estacao-ano-variavel...\n")

base_meta <- unique(
  disp[
    ,
    .(
      cod_estacao,
      nome_estacao,
      tipo_estacao,
      latitude,
      longitude,
      altitude_m,
      periodicidade_fonte,
      ano
    )
  ]
)

wide_pct <- dcast(
  disp[
    ,
    .(
      cod_estacao,
      ano,
      variavel,
      pct_validos
    )
  ],
  cod_estacao + ano ~ variavel,
  value.var = "pct_validos"
)

wide_dias <- dcast(
  disp[
    ,
    .(
      cod_estacao,
      ano,
      variavel,
      n_datas_com_valor
    )
  ],
  cod_estacao + ano ~ variavel,
  value.var = "n_datas_com_valor"
)

variaveis_pct <- setdiff(
  names(wide_pct),
  c("cod_estacao", "ano")
)

variaveis_dias <- setdiff(
  names(wide_dias),
  c("cod_estacao", "ano")
)

setnames(
  wide_dias,
  variaveis_dias,
  paste0(
    variaveis_dias,
    "_n_dias"
  )
)

ea <- merge(
  base_meta,
  wide_pct,
  by = c(
    "cod_estacao",
    "ano"
  ),
  all.x = TRUE
)

ea <- merge(
  ea,
  wide_dias,
  by = c(
    "cod_estacao",
    "ano"
  ),
  all.x = TRUE
)

necessarias <- c(
  "temperatura_ar_c",
  "umidade_relativa_pct",
  "velocidade_vento_ms",
  "direcao_vento_graus",
  "pressao_estacao_hpa"
)

faltantes <- setdiff(
  necessarias,
  names(ea)
)

if (length(faltantes)) {
  stop(
    "Variaveis esperadas ausentes na matriz: ",
    paste(
      faltantes,
      collapse = ", "
    )
  )
}

# ------------------------------------------------------------
# 6. CRUZAR ALVOS X ESTACOES
# ------------------------------------------------------------

cat("Cruzando alvos de MP10 com estacoes INMET...\n")

alvos_cross <- copy(alvos)
ea_cross <- copy(ea)

alvos_cross[
  ,
  chave_tmp := 1L
]

ea_cross[
  ,
  chave_tmp := 1L
]

triagem <- merge(
  alvos_cross,
  ea_cross,
  by = "chave_tmp",
  allow.cartesian = TRUE
)

triagem[
  ,
  chave_tmp := NULL
]

triagem <- triagem[
  ano == ano_alvo
]

triagem[
  ,
  distancia_km := haversine_km(
    latitude_mp10,
    longitude_mp10,
    latitude,
    longitude
  )
]

triagem[
  ,
  faixa_distancia := cut(
    distancia_km,
    breaks = c(
      -Inf,
      25,
      50,
      75,
      100,
      150,
      Inf
    ),
    labels = c(
      "<=25 km",
      ">25-50 km",
      ">50-75 km",
      ">75-100 km",
      ">100-150 km",
      ">150 km"
    )
  )
]

# ------------------------------------------------------------
# 7. COMPLETUDE NUCLEAR E COMPLEMENTAR
# ------------------------------------------------------------

triagem[
  ,
  n_variaveis_nucleares_com_valor :=
    rowSums(
      cbind(
        !is.na(temperatura_ar_c) &
          temperatura_ar_c > 0,
        !is.na(umidade_relativa_pct) &
          umidade_relativa_pct > 0,
        !is.na(velocidade_vento_ms) &
          velocidade_vento_ms > 0,
        !is.na(direcao_vento_graus) &
          direcao_vento_graus > 0
      )
    )
]

triagem[
  ,
  todas_nucleares_com_valor :=
    n_variaveis_nucleares_com_valor == 4L
]

triagem[
  ,
  completude_min_nucleares := pmin(
    temperatura_ar_c,
    umidade_relativa_pct,
    velocidade_vento_ms,
    direcao_vento_graus,
    na.rm = FALSE
  )
]

triagem[
  ,
  completude_media_nucleares := rowMeans(
    cbind(
      temperatura_ar_c,
      umidade_relativa_pct,
      velocidade_vento_ms,
      direcao_vento_graus
    ),
    na.rm = FALSE
  )
]

triagem[
  ,
  pressao_disponivel := (
    !is.na(pressao_estacao_hpa) &
      pressao_estacao_hpa > 0
  )
]

# Indicadores apenas descritivos. Nao constituem regra final.
triagem[
  ,
  classe_completude_nuclear := fifelse(
    !todas_nucleares_com_valor,
    "incompleta",
    fifelse(
      completude_min_nucleares >= 90,
      ">=90%",
      fifelse(
        completude_min_nucleares >= 75,
        "75-<90%",
        fifelse(
          completude_min_nucleares >= 50,
          "50-<75%",
          "<50%"
        )
      )
    )
  )
]

# ------------------------------------------------------------
# 8. RANKINGS SEPARADOS, SEM ESCORE
# ------------------------------------------------------------

setorder(
  triagem,
  estacao_historica,
  ano_alvo,
  tipo_estacao,
  distancia_km
)

triagem[
  ,
  rank_distancia := frank(
    distancia_km,
    ties.method = "min"
  ),
  by = .(
    estacao_historica,
    ano_alvo,
    tipo_estacao
  )
]

triagem[
  ,
  rank_completude_nuclear := frank(
    -completude_min_nucleares,
    ties.method = "min",
    na.last = "keep"
  ),
  by = .(
    estacao_historica,
    ano_alvo,
    tipo_estacao
  )
]

# ------------------------------------------------------------
# 9. SEPARAR AUTOMATICAS E CONVENCIONAIS
# ------------------------------------------------------------

auto <- triagem[
  tipo_estacao == "AUTOMATICA"
]

conv <- triagem[
  tipo_estacao == "CONVENCIONAL"
]

# Lista curta automatica: todas com as quatro variaveis nucleares.
# Mantem todas as distancias; a distancia e analisada explicitamente.
auto_viavel <- auto[
  todas_nucleares_com_valor == TRUE
]

setorder(
  auto_viavel,
  estacao_historica,
  ano_alvo,
  distancia_km
)

# ------------------------------------------------------------
# 10. RESUMO POR ALVO
# ------------------------------------------------------------

resumo_alvos <- auto[
  ,
  {
    z <- .SD[
      todas_nucleares_com_valor == TRUE
    ][
      order(
        distancia_km
      )
    ]

    if (!nrow(z)) {
      list(
        n_automaticas_ano = .N,
        n_automaticas_nucleares_completas = 0L,
        cod_auto_mais_proxima = NA_character_,
        nome_auto_mais_proxima = NA_character_,
        distancia_auto_mais_proxima_km = NA_real_,
        completude_min_auto_mais_proxima = NA_real_,
        pressao_auto_mais_proxima_pct = NA_real_
      )
    } else {
      list(
        n_automaticas_ano = .N,
        n_automaticas_nucleares_completas = nrow(z),
        cod_auto_mais_proxima = z$cod_estacao[1],
        nome_auto_mais_proxima = z$nome_estacao[1],
        distancia_auto_mais_proxima_km = z$distancia_km[1],
        completude_min_auto_mais_proxima =
          z$completude_min_nucleares[1],
        pressao_auto_mais_proxima_pct =
          z$pressao_estacao_hpa[1]
      )
    }
  },
  by = .(
    estacao_historica,
    ano_alvo,
    origem_coordenada,
    status_espacial
  )
]

# ------------------------------------------------------------
# 11. DISPONIBILIDADE 2022 SEM PAREAMENTO ESPACIAL
# ------------------------------------------------------------

disp_2022_auto <- ea[
  ano == 2022L &
    tipo_estacao == "AUTOMATICA"
]

disp_2022_auto[
  ,
  n_variaveis_nucleares_com_valor :=
    rowSums(
      cbind(
        !is.na(temperatura_ar_c) &
          temperatura_ar_c > 0,
        !is.na(umidade_relativa_pct) &
          umidade_relativa_pct > 0,
        !is.na(velocidade_vento_ms) &
          velocidade_vento_ms > 0,
        !is.na(direcao_vento_graus) &
          direcao_vento_graus > 0
      )
    )
]

disp_2022_auto[
  ,
  completude_min_nucleares := pmin(
    temperatura_ar_c,
    umidade_relativa_pct,
    velocidade_vento_ms,
    direcao_vento_graus,
    na.rm = FALSE
  )
]

setorder(
  disp_2022_auto,
  -completude_min_nucleares,
  cod_estacao
)

# ------------------------------------------------------------
# 12. EXPORTAR
# ------------------------------------------------------------

fwrite(
  triagem,
  file.path(
    SAIDA,
    "triagem_todas_estacoes_inmet.csv"
  ),
  bom = TRUE
)

fwrite(
  auto,
  file.path(
    SAIDA,
    "triagem_estacoes_automaticas.csv"
  ),
  bom = TRUE
)

fwrite(
  conv,
  file.path(
    SAIDA,
    "triagem_estacoes_convencionais_secundarias.csv"
  ),
  bom = TRUE
)

fwrite(
  auto_viavel,
  file.path(
    SAIDA,
    "lista_automaticas_com_variaveis_nucleares.csv"
  ),
  bom = TRUE
)

fwrite(
  resumo_alvos,
  file.path(
    SAIDA,
    "resumo_triagem_por_alvo_mp10.csv"
  ),
  bom = TRUE
)

fwrite(
  disp_2022_auto,
  file.path(
    SAIDA,
    "disponibilidade_automaticas_2022_sem_pareamento_suape.csv"
  ),
  bom = TRUE
)

# ------------------------------------------------------------
# 13. RESUMO EXECUCAO
# ------------------------------------------------------------

resumo_exec <- data.table(
  indicador = c(
    "n_alvos_mp10_com_coordenada",
    "n_estacoes_automaticas_avaliadas",
    "n_estacoes_convencionais_avaliadas",
    "n_linhas_triagem_automaticas",
    "n_linhas_triagem_convencionais",
    "n_linhas_auto_com_4_variaveis_nucleares",
    "n_alvos_com_auto_nuclear_disponivel",
    "n_alvos_sem_auto_nuclear_disponivel",
    "suape_2022_pareamento_espacial"
  ),
  valor = c(
    nrow(alvos),
    uniqueN(auto$cod_estacao),
    uniqueN(conv$cod_estacao),
    nrow(auto),
    nrow(conv),
    nrow(auto_viavel),
    sum(
      !is.na(
        resumo_alvos$cod_auto_mais_proxima
      )
    ),
    sum(
      is.na(
        resumo_alvos$cod_auto_mais_proxima
      )
    ),
    0L
  )
)

fwrite(
  resumo_exec,
  file.path(
    SAIDA,
    "resumo_execucao_triagem_multivariavel.csv"
  ),
  bom = TRUE
)

# ------------------------------------------------------------
# 14. CONSOLE
# ------------------------------------------------------------

cat("\n============================================================\n")
cat("ETAPA 9J - TRIAGEM MULTIVARIAVEL INMET CONCLUIDA\n")
cat("============================================================\n\n")

print(resumo_exec)

cat("\nEstacao automatica mais proxima com as 4 variaveis nucleares:\n")

print(
  resumo_alvos[
    order(
      ano_alvo,
      estacao_historica
    )
  ]
)

cat("\nTres automaticas mais proximas por alvo:\n")

top3 <- auto[
  order(
    estacao_historica,
    ano_alvo,
    distancia_km
  ),
  head(
    .SD,
    3
  ),
  by = .(
    estacao_historica,
    ano_alvo
  )
]

print(
  top3[
    ,
    .(
      estacao_historica,
      ano_alvo,
      cod_estacao,
      nome_estacao,
      distancia_km,
      completude_min_nucleares,
      temperatura_ar_c,
      umidade_relativa_pct,
      velocidade_vento_ms,
      direcao_vento_graus,
      pressao_estacao_hpa
    )
  ]
)

cat(
  "\nIMPORTANTE:\n",
  "- proximidade e completude foram mantidas separadas;\n",
  "- nenhuma estacao foi escolhida automaticamente;\n",
  "- estacoes convencionais permanecem apenas como diagnostico/secundarias;\n",
  "- SUAPE 2022 permanece sem pareamento espacial;\n",
  "- a proxima etapa deve decidir se a rede INMET existente e suficiente ou se sera necessario complementar com outra fonte meteorologica.\n",
  sep = ""
)
