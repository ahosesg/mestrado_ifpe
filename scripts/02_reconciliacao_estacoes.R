# ============================================================
# ETAPA 2 - RECONCILIACAO CADASTRAL E ESPACIAL DAS ESTACOES
# Pernambuco, 2017-2025
# ============================================================
#
# Objetivo:
# Reconstruir a identidade cadastral das estacoes utilizadas no
# processamento de MP10, comparar nomes, codigos, IDs, municipios
# e coordenadas entre as bases historicas e o MonitorAr e gerar
# alertas objetivos para mudancas ou inconsistencias espaciais.
#
# IMPORTANTE:
# - Este script NAO altera a base v1.
# - Coordenadas historicas atribuídas na v1 NAO sao tratadas como
#   coordenadas observadas neste diagnostico.
# - Os arquivos MonitorAr sao consultados em todos os parametros,
#   nao apenas MP10, para tentar localizar cadastros de estacoes
#   mesmo quando o poluente nao estiver disponivel naquele ano.
# - O produto desta etapa e diagnostico cadastral. A localizacao
#   final de cada estacao somente deve ser congelada depois da
#   verificacao documental das divergencias encontradas.
# ============================================================

library(dplyr)

if (!requireNamespace("data.table", quietly = TRUE)) {
  stop("Instale o pacote data.table antes de executar este script.")
}

# ------------------------------------------------------------
# 0. CONFIGURACAO
# ------------------------------------------------------------

pasta_saida <- file.path("outputs", "02_estacoes")
dir.create(pasta_saida, recursive = TRUE, showWarnings = FALSE)

arquivos_monitorar <- paste0("Dados_MonitorAr_", 2023:2025, ".csv")
arquivos_historicos <- paste0("PE", 2017:2022, ".csv")
arquivo_v1 <- "dados_MP10_2017_2025_PE_v1.rds"

# ------------------------------------------------------------
# FUNCOES AUXILIARES
# ------------------------------------------------------------

normalizar_texto <- function(x) {
  x <- as.character(x)
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  x <- toupper(trimws(x))
  x <- gsub("[^A-Z0-9]+", " ", x)
  x <- gsub("\\s+", " ", x)
  trimws(x)
}

normalizar_nome_estacao <- function(x) {
  y <- normalizar_texto(x)
  # Remove apenas palavras genericas que frequentemente aparecem
  # como prefixo/sufixo cadastral, preservando o nucleo do nome.
  y <- gsub("\\bESTACAO\\b", "", y)
  y <- gsub("\\bQUALIDADE DO AR\\b", "", y)
  y <- gsub("\\bMONITORAMENTO\\b", "", y)
  y <- gsub("\\s+", " ", y)
  trimws(y)
}

haversine_km <- function(lat1, lon1, lat2, lon2) {
  ok <- !is.na(lat1) & !is.na(lon1) & !is.na(lat2) & !is.na(lon2)
  out <- rep(NA_real_, length(lat1))
  if (!any(ok)) return(out)

  rad <- pi / 180
  r <- 6371.0088
  phi1 <- lat1[ok] * rad
  phi2 <- lat2[ok] * rad
  dphi <- (lat2[ok] - lat1[ok]) * rad
  dlambda <- (lon2[ok] - lon1[ok]) * rad

  a <- sin(dphi / 2)^2 + cos(phi1) * cos(phi2) * sin(dlambda / 2)^2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  out[ok] <- r * c
  out
}

colapsar_unicos <- function(x) {
  x <- unique(as.character(x[!is.na(x) & trimws(as.character(x)) != ""]))
  if (length(x) == 0) return(NA_character_)
  paste(sort(x), collapse = " | ")
}

# ------------------------------------------------------------
# 1. CADASTRO HISTORICO 2017-2022
# ------------------------------------------------------------

ler_historico <- function(arq) {
  if (!file.exists(arq)) {
    warning(paste("Arquivo historico nao encontrado:", arq))
    return(NULL)
  }

  ano_ref <- as.integer(gsub("[^0-9]", "", arq))
  x <- data.table::fread(
    arq,
    select = intersect(
      c("Estacao", "Codigo", "Poluente", "Unidade", "Tipo"),
      names(data.table::fread(arq, nrows = 0))
    ),
    showProgress = FALSE
  )
  x <- as.data.frame(x)

  if (!all(c("Estacao", "Codigo", "Poluente") %in% names(x))) {
    warning(paste("Estrutura insuficiente em", arq))
    return(NULL)
  }

  if (!"Unidade" %in% names(x)) x$Unidade <- NA_character_
  if (!"Tipo" %in% names(x)) x$Tipo <- NA_character_

  x %>%
    filter(Poluente == "MP10") %>%
    mutate(
      ano = ano_ref,
      arquivo_origem = arq,
      nome_normalizado = normalizar_nome_estacao(Estacao),
      codigo_historico = as.character(Codigo)
    ) %>%
    group_by(
      ano, arquivo_origem, Estacao, nome_normalizado,
      codigo_historico, Unidade, Tipo
    ) %>%
    summarise(n_registros = n(), .groups = "drop")
}

cadastro_historico <- bind_rows(lapply(arquivos_historicos, ler_historico))

write.csv(
  cadastro_historico,
  file.path(pasta_saida, "cadastro_historico_mp10_2017_2022.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# ------------------------------------------------------------
# 2. CADASTRO MONITORAR 2023-2025, TODOS OS PARAMETROS
# ------------------------------------------------------------

ler_monitorar_pe <- function(arq) {
  if (!file.exists(arq)) {
    warning(paste("Arquivo MonitorAr nao encontrado:", arq))
    return(NULL)
  }

  ano_ref <- as.integer(gsub("[^0-9]", "", arq))
  cab <- names(data.table::fread(arq, nrows = 0))

  desejadas <- c(
    "sg_uf", "id_estacao", "cod_estacao", "no_estacao",
    "nu_latitude", "nu_longitude", "id_municipio", "no_municipio",
    "no_fonte_dados", "st_estacao", "cd_normalizado",
    "no_item_monitorado"
  )

  selecionar <- intersect(desejadas, cab)

  x <- data.table::fread(
    arq,
    select = selecionar,
    showProgress = TRUE
  )
  x <- as.data.frame(x)

  if (!"sg_uf" %in% names(x)) {
    warning(paste("sg_uf ausente em", arq))
    return(NULL)
  }

  # Cria colunas ausentes para manter o mesmo esquema entre anos.
  for (nm in desejadas) {
    if (!nm %in% names(x)) x[[nm]] <- NA
  }

  x %>%
    filter(sg_uf == "PE") %>%
    mutate(
      ano = ano_ref,
      arquivo_origem = arq,
      nome_normalizado = normalizar_nome_estacao(no_estacao),
      nu_latitude = suppressWarnings(as.numeric(nu_latitude)),
      nu_longitude = suppressWarnings(as.numeric(nu_longitude)),
      cod_estacao = as.character(cod_estacao),
      id_estacao = as.character(id_estacao),
      id_municipio = as.character(id_municipio)
    )
}

monitorar_pe_bruto <- bind_rows(lapply(arquivos_monitorar, ler_monitorar_pe))

# Uma linha por configuracao cadastral distinta da estacao.
cadastro_monitorar_pe <- monitorar_pe_bruto %>%
  group_by(
    ano, arquivo_origem, id_estacao, cod_estacao, no_estacao,
    nome_normalizado, nu_latitude, nu_longitude,
    id_municipio, no_municipio, no_fonte_dados, st_estacao
  ) %>%
  summarise(
    n_registros = n(),
    n_parametros = n_distinct(cd_normalizado[!is.na(cd_normalizado)]),
    parametros = colapsar_unicos(cd_normalizado),
    .groups = "drop"
  ) %>%
  arrange(ano, no_municipio, no_estacao, id_estacao)

write.csv(
  cadastro_monitorar_pe,
  file.path(pasta_saida, "cadastro_monitorar_pe_2023_2025.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# Cadastro restrito a MP10.
cadastro_monitorar_mp10 <- monitorar_pe_bruto %>%
  filter(cd_normalizado == "MP10") %>%
  group_by(
    ano, arquivo_origem, id_estacao, cod_estacao, no_estacao,
    nome_normalizado, nu_latitude, nu_longitude,
    id_municipio, no_municipio, no_fonte_dados, st_estacao
  ) %>%
  summarise(n_registros_mp10 = n(), .groups = "drop") %>%
  arrange(ano, no_estacao, id_estacao)

write.csv(
  cadastro_monitorar_mp10,
  file.path(pasta_saida, "cadastro_monitorar_pe_mp10_2023_2025.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# ------------------------------------------------------------
# 3. BUSCA AMPLA DAS ESTACOES-ALVO EM TODOS OS PARAMETROS
# ------------------------------------------------------------

palavras_alvo <- c("IPOJUCA", "CUPE", "IFPE", "CPRH", "SUAPE", "GAIBU")
padrao_alvo <- paste(palavras_alvo, collapse = "|")

busca_estacoes_alvo <- monitorar_pe_bruto %>%
  filter(grepl(padrao_alvo, nome_normalizado, ignore.case = TRUE)) %>%
  group_by(
    ano, id_estacao, cod_estacao, no_estacao, nome_normalizado,
    nu_latitude, nu_longitude, no_municipio,
    no_fonte_dados, st_estacao
  ) %>%
  summarise(
    n_registros = n(),
    parametros = colapsar_unicos(cd_normalizado),
    .groups = "drop"
  ) %>%
  arrange(nome_normalizado, ano, id_estacao)

write.csv(
  busca_estacoes_alvo,
  file.path(pasta_saida, "busca_estacoes_alvo_todos_parametros.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# ------------------------------------------------------------
# 4. CROSSWALK HISTORICO x MONITORAR POR NOME NORMALIZADO
# ------------------------------------------------------------

historico_unico <- cadastro_historico %>%
  group_by(codigo_historico, Estacao, nome_normalizado) %>%
  summarise(
    primeiro_ano = min(ano),
    ultimo_ano = max(ano),
    anos = paste(sort(unique(ano)), collapse = ";"),
    .groups = "drop"
  )

monitorar_unico <- cadastro_monitorar_pe %>%
  group_by(
    id_estacao, cod_estacao, no_estacao, nome_normalizado,
    no_municipio
  ) %>%
  summarise(
    primeiro_ano_monitorar = min(ano),
    ultimo_ano_monitorar = max(ano),
    anos_monitorar = paste(sort(unique(ano)), collapse = ";"),
    latitude_min = suppressWarnings(min(nu_latitude, na.rm = TRUE)),
    latitude_max = suppressWarnings(max(nu_latitude, na.rm = TRUE)),
    longitude_min = suppressWarnings(min(nu_longitude, na.rm = TRUE)),
    longitude_max = suppressWarnings(max(nu_longitude, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  mutate(
    latitude_min = ifelse(is.infinite(latitude_min), NA_real_, latitude_min),
    latitude_max = ifelse(is.infinite(latitude_max), NA_real_, latitude_max),
    longitude_min = ifelse(is.infinite(longitude_min), NA_real_, longitude_min),
    longitude_max = ifelse(is.infinite(longitude_max), NA_real_, longitude_max)
  )

crosswalk_exato <- historico_unico %>%
  left_join(
    monitorar_unico,
    by = "nome_normalizado",
    suffix = c("_historico", "_monitorar")
  ) %>%
  mutate(
    status_match = case_when(
      !is.na(id_estacao) ~ "match_exato_nome_normalizado",
      TRUE ~ "sem_match_exato"
    )
  )

write.csv(
  crosswalk_exato,
  file.path(pasta_saida, "crosswalk_historico_monitorar.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# ------------------------------------------------------------
# 5. COORDENADAS OBSERVADAS POR ESTACAO E ANO NO MONITORAR
# ------------------------------------------------------------

coordenadas_monitorar <- cadastro_monitorar_pe %>%
  filter(!is.na(nu_latitude), !is.na(nu_longitude)) %>%
  group_by(
    ano, id_estacao, cod_estacao, no_estacao,
    nome_normalizado, no_municipio
  ) %>%
  summarise(
    n_pares_coordenadas = n_distinct(
      paste(round(nu_latitude, 6), round(nu_longitude, 6), sep = ";")
    ),
    latitude_mediana = median(nu_latitude, na.rm = TRUE),
    longitude_mediana = median(nu_longitude, na.rm = TRUE),
    latitude_min = min(nu_latitude, na.rm = TRUE),
    latitude_max = max(nu_latitude, na.rm = TRUE),
    longitude_min = min(nu_longitude, na.rm = TRUE),
    longitude_max = max(nu_longitude, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(nome_normalizado, ano, id_estacao)

write.csv(
  coordenadas_monitorar,
  file.path(pasta_saida, "coordenadas_monitorar_estacao_ano.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# ------------------------------------------------------------
# 6. DESLOCAMENTOS ENTRE ANOS CONSECUTIVOS DISPONIVEIS
# ------------------------------------------------------------

# O pareamento e feito pelo nome normalizado. Se houver mais de um
# ID para o mesmo nome em um ano, cada cadastro permanece separado
# e o resultado deve ser interpretado como alerta cadastral.

deslocamentos <- coordenadas_monitorar %>%
  group_by(nome_normalizado) %>%
  arrange(ano, id_estacao, .by_group = TRUE) %>%
  mutate(
    ano_anterior = lag(ano),
    id_estacao_anterior = lag(id_estacao),
    latitude_anterior = lag(latitude_mediana),
    longitude_anterior = lag(longitude_mediana),
    distancia_km = haversine_km(
      latitude_anterior,
      longitude_anterior,
      latitude_mediana,
      longitude_mediana
    )
  ) %>%
  ungroup() %>%
  filter(!is.na(ano_anterior)) %>%
  mutate(
    alerta_deslocamento = case_when(
      distancia_km >= 1 ~ "ALERTA >= 1 km",
      distancia_km >= 0.25 ~ "VERIFICAR 250 m a 1 km",
      TRUE ~ "sem_alerta_por_distancia"
    )
  )

write.csv(
  deslocamentos,
  file.path(pasta_saida, "deslocamentos_estacoes_monitorar.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# ------------------------------------------------------------
# 7. COORDENADAS COMPARTILHADAS POR ESTACOES DISTINTAS
# ------------------------------------------------------------

coordenadas_compartilhadas <- cadastro_monitorar_pe %>%
  filter(!is.na(nu_latitude), !is.na(nu_longitude)) %>%
  mutate(
    latitude_6 = round(nu_latitude, 6),
    longitude_6 = round(nu_longitude, 6)
  ) %>%
  group_by(ano, latitude_6, longitude_6) %>%
  summarise(
    n_ids_estacao = n_distinct(id_estacao),
    n_nomes_estacao = n_distinct(no_estacao),
    ids_estacao = colapsar_unicos(id_estacao),
    nomes_estacao = colapsar_unicos(no_estacao),
    municipios = colapsar_unicos(no_municipio),
    .groups = "drop"
  ) %>%
  filter(n_ids_estacao > 1 | n_nomes_estacao > 1) %>%
  arrange(ano, desc(n_ids_estacao), latitude_6, longitude_6)

write.csv(
  coordenadas_compartilhadas,
  file.path(pasta_saida, "coordenadas_compartilhadas_estacoes.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# ------------------------------------------------------------
# 8. COMPARACAO COM AS COORDENADAS PRESENTES NA V1
# ------------------------------------------------------------

if (file.exists(arquivo_v1)) {
  v1 <- readRDS(arquivo_v1)

  if (all(c(
    "ano", "cod_estacao", "no_estacao", "nu_latitude",
    "nu_longitude", "origem_coordenada"
  ) %in% names(v1))) {

    coordenadas_v1 <- v1 %>%
      mutate(
        ano = as.integer(ano),
        cod_estacao = as.character(cod_estacao),
        nome_normalizado = normalizar_nome_estacao(no_estacao)
      ) %>%
      group_by(
        ano, cod_estacao, no_estacao, nome_normalizado,
        origem_coordenada
      ) %>%
      summarise(
        n_pares = n_distinct(
          paste(round(nu_latitude, 6), round(nu_longitude, 6), sep = ";")
        ),
        latitude_mediana = ifelse(
          all(is.na(nu_latitude)), NA_real_, median(nu_latitude, na.rm = TRUE)
        ),
        longitude_mediana = ifelse(
          all(is.na(nu_longitude)), NA_real_, median(nu_longitude, na.rm = TRUE)
        ),
        .groups = "drop"
      ) %>%
      arrange(no_estacao, ano)

    write.csv(
      coordenadas_v1,
      file.path(pasta_saida, "coordenadas_v1_estacao_ano.csv"),
      row.names = FALSE,
      fileEncoding = "UTF-8"
    )
  }
}

# ------------------------------------------------------------
# 9. RESUMO DE ALERTAS PARA DECISAO MANUAL/DOCUMENTAL
# ------------------------------------------------------------

alertas_deslocamento <- deslocamentos %>%
  filter(!is.na(distancia_km), distancia_km >= 0.25) %>%
  transmute(
    tipo_alerta = "deslocamento_cadastral",
    ano,
    estacao = no_estacao,
    id_estacao,
    descricao = paste0(
      "Distancia de ", round(distancia_km, 3),
      " km em relacao ao cadastro anterior (ano ", ano_anterior, ")."
    )
  )

alertas_compartilhamento <- coordenadas_compartilhadas %>%
  transmute(
    tipo_alerta = "coordenada_compartilhada",
    ano,
    estacao = nomes_estacao,
    id_estacao = ids_estacao,
    descricao = paste0(
      "Mesma coordenada para ", n_ids_estacao,
      " IDs e ", n_nomes_estacao, " nomes de estacao."
    )
  )

alertas_reconciliacao <- bind_rows(
  alertas_deslocamento,
  alertas_compartilhamento
) %>%
  arrange(ano, tipo_alerta, estacao)

write.csv(
  alertas_reconciliacao,
  file.path(pasta_saida, "alertas_reconciliacao_estacoes.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# ------------------------------------------------------------
# 10. RESUMO DA EXECUCAO
# ------------------------------------------------------------

resumo_execucao <- data.frame(
  indicador = c(
    "estacoes_historicas_mp10",
    "estacoes_monitorar_pe_todos_parametros",
    "estacoes_monitorar_pe_mp10",
    "linhas_busca_estacoes_alvo",
    "alertas_deslocamento_>=250m",
    "coordenadas_compartilhadas",
    "ano_2023_tem_registros_pe"
  ),
  valor = c(
    n_distinct(cadastro_historico$nome_normalizado),
    n_distinct(cadastro_monitorar_pe$nome_normalizado),
    n_distinct(cadastro_monitorar_mp10$nome_normalizado),
    nrow(busca_estacoes_alvo),
    nrow(alertas_deslocamento),
    nrow(coordenadas_compartilhadas),
    any(cadastro_monitorar_pe$ano == 2023)
  )
)

write.csv(
  resumo_execucao,
  file.path(pasta_saida, "resumo_reconciliacao_estacoes.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

cat("\n============================================================\n")
cat("ETAPA 2 - RECONCILIACAO CADASTRAL CONCLUIDA\n")
cat("============================================================\n")
cat("Saidas em:", pasta_saida, "\n\n")
print(resumo_execucao)
cat("\nArquivos produzidos:\n")
print(list.files(pasta_saida))
