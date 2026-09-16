# ============================================================
# PREPARAÇÃO E PADRONIZAÇÃO DA BASE HISTÓRICA DE MP10
# PERNAMBUCO, 2017–2025
# ============================================================
#
# Objetivo:
# Construir uma base única e padronizada de concentrações
# horárias de MP10 para Pernambuco, abrangendo o período de
# 2017 a 2025 e integrando dados oriundos de diferentes
# estruturas de banco de dados.
#
# As bases utilizadas apresentam duas estruturas distintas:
#
# 1. Bases PE2017 a PE2022:
#    - específicas do estado de Pernambuco;
#    - contêm diferentes poluentes;
#    - possuem estrutura simplificada, com as variáveis:
#      Data, Hora, Estacao, Codigo, Poluente, Valor,
#      Unidade e Tipo.
#
# 2. Bases Dados_MonitorAr_2023 a Dados_MonitorAr_2025:
#    - abrangem diferentes unidades federativas;
#    - contêm diferentes poluentes;
#    - apresentam estrutura mais detalhada e padronizada.
#
# Como referência para a integração, foi adotada a estrutura
# das bases MonitorAr.
#
# O procedimento contempla:
#
# 1. carregamento dos pacotes;
# 2. importação das bases;
# 3. seleção dos registros de MP10;
# 4. seleção espacial dos registros de Pernambuco;
# 5. harmonização da nomenclatura das variáveis;
# 6. padronização das bases de 2017 a 2022;
# 7. identificação do ano de referência;
# 8. integração das bases anuais;
# 9. normalização dos nomes e códigos das estações;
# 10. separação das informações de data e hora;
# 11. padronização dos horários de medição;
# 12. criação das variáveis temporais auxiliares;
# 13. verificação da consistência da base consolidada;
# 14. exportação da base processada.
#
# O procedimento mantém, sempre que possível, as informações
# originais e cria novas variáveis para as informações
# padronizadas, assegurando rastreabilidade das transformações.
# ============================================================



# ------------------------------------------------------------
# 1. CARREGAMENTO DO PACOTE NECESSÁRIO
# ------------------------------------------------------------

# O pacote dplyr é utilizado para manipulação dos dados,
# especialmente nas operações de:
#
# - filtragem de observações;
# - criação e transformação de variáveis;
# - agrupamento;
# - identificação de registros distintos;
# - integração das bases.
#
# O uso de comandos programados permite que todas as etapas
# de preparação sejam reproduzidas posteriormente.

library(dplyr)



# ------------------------------------------------------------
# 2. IMPORTAÇÃO DAS BASES DE DADOS
# ------------------------------------------------------------

# Foram utilizadas nove bases anuais referentes ao período
# de 2017 a 2025.
#
# As bases PE2017 a PE2022 já são territorialmente referentes
# ao estado de Pernambuco, mas contêm registros de diferentes
# poluentes atmosféricos.
#
# As bases MonitorAr de 2023 a 2025 contêm simultaneamente
# registros de diferentes estados brasileiros e diferentes
# parâmetros monitorados. Por esse motivo, posteriormente serão
# aplicados critérios de seleção territorial e por poluente.


# Bases de Pernambuco, 2017–2022

PE2017 <- read.csv("PE2017.csv")

PE2018 <- read.csv("PE2018.csv")

PE2019 <- read.csv("PE2019.csv")

PE2020 <- read.csv("PE2020.csv")

PE2021 <- read.csv("PE2021.csv")

PE2022 <- read.csv("PE2022.csv")


# Bases MonitorAr, 2023–2025

Dados_MonitorAr_2023 <- read.csv(
  "Dados_MonitorAr_2023.csv"
)

Dados_MonitorAr_2024 <- read.csv(
  "Dados_MonitorAr_2024.csv"
)

Dados_MonitorAr_2025 <- read.csv(
  "Dados_MonitorAr_2025.csv"
)



# ------------------------------------------------------------
# 3. SELEÇÃO DOS REGISTROS DE MP10 NAS BASES DE 2017 A 2022
# ------------------------------------------------------------

# As bases PE2017 a PE2022 já correspondem ao estado de
# Pernambuco. Portanto, não é necessário realizar nova
# filtragem pela unidade federativa.
#
# Entretanto, essas bases apresentam diferentes poluentes,
# como CO, MP10, NO2, O3 e SO2.
#
# Como o objeto de análise deste estudo é o material particulado
# com diâmetro aerodinâmico de até 10 µm (MP10), foram
# selecionadas exclusivamente as observações em que a variável
# "Poluente" é igual a "MP10".


dados2017PE_MP10 <- PE2017 %>%
  filter(Poluente == "MP10")

dados2018PE_MP10 <- PE2018 %>%
  filter(Poluente == "MP10")

dados2019PE_MP10 <- PE2019 %>%
  filter(Poluente == "MP10")

dados2020PE_MP10 <- PE2020 %>%
  filter(Poluente == "MP10")

dados2021PE_MP10 <- PE2021 %>%
  filter(Poluente == "MP10")

dados2022PE_MP10 <- PE2022 %>%
  filter(Poluente == "MP10")



# ------------------------------------------------------------
# 3.1 VERIFICAÇÃO DA SELEÇÃO DO POLUENTE
# ------------------------------------------------------------

# A quantidade de registros resultante em cada ano é verificada
# para identificar eventuais anos sem dados ou com disponibilidade
# reduzida de observações.

nrow(dados2017PE_MP10)

nrow(dados2018PE_MP10)

nrow(dados2019PE_MP10)

nrow(dados2020PE_MP10)

nrow(dados2021PE_MP10)

nrow(dados2022PE_MP10)


# Também é verificado se, após a filtragem, apenas o poluente
# MP10 permanece em cada base.

table(dados2017PE_MP10$Poluente)

table(dados2018PE_MP10$Poluente)

table(dados2019PE_MP10$Poluente)

table(dados2020PE_MP10$Poluente)

table(dados2021PE_MP10$Poluente)

table(dados2022PE_MP10$Poluente)



# ------------------------------------------------------------
# 4. SELEÇÃO DOS REGISTROS DE PERNAMBUCO E MP10
#    NAS BASES MONITORAR DE 2023 A 2025
# ------------------------------------------------------------

# As bases MonitorAr apresentam simultaneamente:
#
# - diferentes unidades federativas;
# - diferentes poluentes e parâmetros meteorológicos.
#
# Portanto, foram utilizados dois critérios simultâneos:
#
# sg_uf == "PE"
#   seleciona registros atribuídos ao estado de Pernambuco.
#
# cd_normalizado == "MP10"
#   seleciona registros classificados pelo MonitorAr como MP10.


dados2023PE_MP10 <- Dados_MonitorAr_2023 %>%
  filter(
    sg_uf == "PE",
    cd_normalizado == "MP10"
  )


dados2024PE_MP10 <- Dados_MonitorAr_2024 %>%
  filter(
    sg_uf == "PE",
    cd_normalizado == "MP10"
  )


dados2025PE_MP10 <- Dados_MonitorAr_2025 %>%
  filter(
    sg_uf == "PE",
    cd_normalizado == "MP10"
  )



# ------------------------------------------------------------
# 4.1 VERIFICAÇÃO DA QUANTIDADE DE REGISTROS
# ------------------------------------------------------------

# Essa conferência permite identificar imediatamente situações
# em que determinada combinação de estado e poluente não esteja
# disponível na base de origem.

nrow(dados2023PE_MP10)

nrow(dados2024PE_MP10)

nrow(dados2025PE_MP10)


# ATENÇÃO:
#
# Na inspeção realizada anteriormente, a base MonitorAr de 2023
# não apresentou registros com sg_uf == "PE".
#
# Caso nrow(dados2023PE_MP10) retorne zero, o ano de 2023
# permanecerá sem observações na série consolidada e deverá ser
# tratado como uma lacuna de disponibilidade dos dados, até que
# sua origem seja esclarecida.



# ------------------------------------------------------------
# 5. HARMONIZAÇÃO DO NOME DE UMA VARIÁVEL DA BASE DE 2024
# ------------------------------------------------------------

# Foi identificada uma diferença na nomenclatura de uma variável
# entre as bases MonitorAr.
#
# Na base de 2024:
#
# st_situacao_item
#
# Nas demais bases utilizadas:
#
# st_situacao
#
# Como ambas ocupam a mesma posição e representam o campo
# correspondente à situação do item, o nome da variável de 2024
# é harmonizado antes da integração.

names(dados2024PE_MP10)[
  names(dados2024PE_MP10) == "st_situacao_item"
] <- "st_situacao"



# ------------------------------------------------------------
# 6. PADRONIZAÇÃO DAS BASES HISTÓRICAS DE 2017 A 2022
# ------------------------------------------------------------

# As bases de 2017 a 2022 apresentam exatamente a mesma
# estrutura:
#
# Data
# Hora
# Estacao
# Codigo
# Poluente
# Valor
# Unidade
# Tipo
#
# Essa estrutura é diferente da estrutura utilizada pelo
# MonitorAr entre 2023 e 2025.
#
# Para possibilitar a integração vertical das séries, foi
# adotada a estrutura MonitorAr como estrutura de referência.
#
# As correspondências utilizadas são:
#
# Data + Hora  -> dh_medicao
#
# Valor        -> nu_concentracao
#
# Poluente     -> no_item_monitorado e cd_normalizado
#
# Codigo       -> cod_estacao
#
# Estacao      -> no_estacao
#
# As variáveis existentes nas bases MonitorAr, mas sem
# correspondência disponível nas bases de 2017 a 2022, recebem
# NA.
#
# O uso de NA é intencional: significa que a informação não está
# disponível na fonte original. Dessa forma, evita-se imputar
# artificialmente informações inexistentes.
#
# Como o procedimento é idêntico para seis anos consecutivos,
# foi criada uma função denominada "padronizar_base_antiga".
#
# Essa estratégia possui duas vantagens:
#
# 1. garante que exatamente o mesmo tratamento seja aplicado
#    a todos os anos de 2017 a 2022;
#
# 2. reduz a possibilidade de erros decorrentes da repetição
#    manual de blocos de código.


padronizar_base_antiga <- function(dados, ano_ref) {
  
  data.frame(
    
    # Informação específica da fonte de dados não disponível
    # nas bases históricas.
    no_fonte_dados = NA_character_,
    
    
    # Criação de uma variável única contendo data e hora.
    #
    # Exemplo:
    #
    # Data = 2019-01-01
    # Hora = 10:00:00
    #
    # resultado:
    #
    # dh_medicao = 2019-01-01 10:00:00
    
    dh_medicao = paste(
      dados$Data,
      dados$Hora
    ),
    
    
    # Índice de qualidade do ar não disponível na estrutura
    # histórica.
    
    nu_iqar = NA_real_,
    
    
    # Correspondência direta entre o campo "Valor" da estrutura
    # histórica e a concentração do parâmetro monitorado.
    
    nu_concentracao = dados$Valor,
    
    
    # Variáveis relacionadas à situação e validação dos registros
    # que não possuem equivalência direta nas bases antigas.
    
    st_situacao = NA_character_,
    
    cd_flag = NA_character_,
    
    ds_flag = NA_character_,
    
    st_situacao_iqar = NA_character_,
    
    
    # Como os dados já foram previamente filtrados para MP10,
    # o nome do item monitorado é explicitamente padronizado.
    
    no_item_monitorado =
      "Material Particulado com diâmetro de até 10 μm",
    
    
    # Código normalizado do poluente.
    
    cd_normalizado = "MP10",
    
    
    # Identificador interno MonitorAr inexistente nas bases
    # históricas.
    
    id_estacao = NA_integer_,
    
    
    # Código original da estação de monitoramento.
    
    cod_estacao = dados$Codigo,
    
    
    # Nome original da estação de monitoramento.
    
    no_estacao = dados$Estacao,
    
    
    # As coordenadas geográficas não estão disponíveis nas bases
    # de 2017 a 2022 neste estágio do processamento.
    
    nu_latitude = NA_real_,
    
    nu_longitude = NA_real_,
    
    
    # Situação operacional da estação não disponível.
    
    st_estacao = NA_character_,
    
    
    # Identificador MonitorAr do município não disponível.
    
    id_municipio = NA_integer_,
    
    
    # Nome do município não disponível diretamente na estrutura
    # histórica.
    
    no_municipio = NA_character_,
    
    
    # Como todas as bases PE2017–PE2022 são referentes a
    # Pernambuco, a unidade federativa é registrada como "PE".
    
    sg_uf = "PE",
    
    
    # O ano é recebido pela função por meio do argumento ano_ref.
    
    ano = ano_ref
  )
}



# ------------------------------------------------------------
# 7. APLICAÇÃO DA PADRONIZAÇÃO ÀS BASES DE 2017 A 2022
# ------------------------------------------------------------

# A função definida anteriormente é aplicada individualmente
# a cada base anual.
#
# O primeiro argumento corresponde à base de MP10 daquele ano.
#
# O segundo argumento corresponde ao ano de referência.


dados2017PE_MP10_pad <- padronizar_base_antiga(
  dados2017PE_MP10,
  2017
)


dados2018PE_MP10_pad <- padronizar_base_antiga(
  dados2018PE_MP10,
  2018
)


dados2019PE_MP10_pad <- padronizar_base_antiga(
  dados2019PE_MP10,
  2019
)


dados2020PE_MP10_pad <- padronizar_base_antiga(
  dados2020PE_MP10,
  2020
)


dados2021PE_MP10_pad <- padronizar_base_antiga(
  dados2021PE_MP10,
  2021
)


dados2022PE_MP10_pad <- padronizar_base_antiga(
  dados2022PE_MP10,
  2022
)



# ------------------------------------------------------------
# 8. IDENTIFICAÇÃO DO ANO NAS BASES MONITORAR
# ------------------------------------------------------------

# As bases MonitorAr também recebem uma variável explícita
# denominada "ano".
#
# Embora o ano já possa ser identificado a partir de dh_medicao,
# sua criação como variável independente facilita:
#
# - agrupamentos;
# - estatísticas anuais;
# - análise da cobertura temporal;
# - identificação de lacunas;
# - comparação entre anos.

dados2023PE_MP10 <- dados2023PE_MP10 %>%
  mutate(
    ano = 2023
  )


dados2024PE_MP10 <- dados2024PE_MP10 %>%
  mutate(
    ano = 2024
  )


dados2025PE_MP10 <- dados2025PE_MP10 %>%
  mutate(
    ano = 2025
  )



# ------------------------------------------------------------
# 9. INTEGRAÇÃO DAS BASES HISTÓRICAS
# ------------------------------------------------------------

# Após a harmonização da estrutura, as nove bases anuais são
# integradas verticalmente utilizando bind_rows().
#
# Cada linha da nova tabela permanece representando uma
# observação individual de MP10.
#
# A variável "ano" permite identificar o período de origem
# de cada registro.
#
# A tabela consolidada passa a ser denominada:
#
# dados_MP10_2017_2025_PE

dados_MP10_2017_2025_PE <- bind_rows(
  
  dados2017PE_MP10_pad,
  
  dados2018PE_MP10_pad,
  
  dados2019PE_MP10_pad,
  
  dados2020PE_MP10_pad,
  
  dados2021PE_MP10_pad,
  
  dados2022PE_MP10_pad,
  
  dados2023PE_MP10,
  
  dados2024PE_MP10,
  
  dados2025PE_MP10
)



# ------------------------------------------------------------
# 10. NORMALIZAÇÃO DOS NOMES E CÓDIGOS DAS ESTAÇÕES
# ------------------------------------------------------------

# A inspeção das bases históricas revelou diferenças de
# nomenclatura para estações equivalentes.
#
# Foram encontrados, por exemplo:
#
# IPOJUCA / Ipojuca
#
# CUPE / EDCUPE
#
# SUAPE / Suape
#
# Se essas diferenças fossem mantidas, funções de agrupamento
# poderiam interpretar uma mesma estação como diferentes
# unidades de monitoramento.
#
# Dessa forma, foi estabelecida uma nomenclatura única.
#
# Também foram harmonizados os códigos das estações, utilizando
# uma codificação única na série histórica:
#
# 12 = IPOJUCA
# 13 = CUPE
# 14 = IFPE
# 15 = CPRH
# 16 = SUAPE
#
# A normalização é realizada com base no nome da estação,
# permitindo também substituir os códigos históricos PE02,
# PE03, PE04, PE05 e PE06 pelos respectivos códigos
# padronizados utilizados na série consolidada.


dados_MP10_2017_2025_PE <- dados_MP10_2017_2025_PE %>%
  mutate(
    
    # Padronização da nomenclatura.
    
    no_estacao = case_when(
      
      no_estacao %in%
        c("IPOJUCA", "Ipojuca") ~ "IPOJUCA",
      
      no_estacao %in%
        c("CUPE", "EDCUPE") ~ "CUPE",
      
      no_estacao == "IFPE" ~ "IFPE",
      
      no_estacao == "CPRH" ~ "CPRH",
      
      no_estacao %in%
        c("SUAPE", "Suape") ~ "SUAPE",
      
      TRUE ~ no_estacao
    ),
    
    
    # Padronização dos códigos.
    
    cod_estacao = case_when(
      
      no_estacao == "IPOJUCA" ~ "12",
      
      no_estacao == "CUPE" ~ "13",
      
      no_estacao == "IFPE" ~ "14",
      
      no_estacao == "CPRH" ~ "15",
      
      no_estacao == "SUAPE" ~ "16",
      
      TRUE ~ cod_estacao
    )
  )

# ------------------------------------------------------------
# 11. VALIDAÇÃO E PADRONIZAÇÃO DAS COORDENADAS GEOGRÁFICAS
# ------------------------------------------------------------

# As bases históricas de 2017 a 2022 não apresentam informações
# de latitude e longitude das estações de monitoramento.
#
# As bases MonitorAr de 2023 a 2025, por outro lado, apresentam
# as variáveis:
#
# nu_latitude
# nu_longitude
#
# Como as estações foram previamente normalizadas por nome e
# código, é possível utilizar as informações geográficas das
# bases mais recentes como referência para as mesmas estações
# existentes nos períodos anteriores.
#
# Antes dessa atribuição, entretanto, são realizadas verificações
# destinadas a confirmar:
#
# 1. se latitude e longitude estão armazenadas como valores
#    numéricos;
#
# 2. se os valores se encontram dentro dos limites possíveis
#    para coordenadas geográficas;
#
# 3. se a mesma estação apresenta coordenadas consistentes entre
#    os registros disponíveis;
#
# 4. se existem valores ausentes ou potencialmente inconsistentes.
#
# A atribuição das coordenadas aos anos antigos somente deve ser
# realizada após essas verificações.



# ------------------------------------------------------------
# 11.1 VERIFICAR O TIPO DAS VARIÁVEIS
# ------------------------------------------------------------

# Para utilização como coordenadas geográficas em graus decimais,
# latitude e longitude devem estar armazenadas como variáveis
# numéricas.

class(
  dados_MP10_2017_2025_PE$nu_latitude
)

class(
  dados_MP10_2017_2025_PE$nu_longitude
)


# O resultado esperado para ambas é:
#
# "numeric"



# ------------------------------------------------------------
# 11.2 VERIFICAR A FAIXA DOS VALORES
# ------------------------------------------------------------

# Em coordenadas geográficas expressas em graus decimais:
#
# latitude deve variar entre -90 e +90 graus;
#
# longitude deve variar entre -180 e +180 graus.
#
# Valores fora dessas faixas indicariam erro de formato,
# transformação ou registro.

range(
  dados_MP10_2017_2025_PE$nu_latitude,
  na.rm = TRUE
)

range(
  dados_MP10_2017_2025_PE$nu_longitude,
  na.rm = TRUE
)



# ------------------------------------------------------------
# 11.3 IDENTIFICAR COORDENADAS FORA DOS LIMITES GEOGRÁFICOS
# ------------------------------------------------------------

coordenadas_invalidas <- dados_MP10_2017_2025_PE %>%
  filter(
    !is.na(nu_latitude),
    !is.na(nu_longitude),
    (
      nu_latitude < -90 |
        nu_latitude > 90 |
        nu_longitude < -180 |
        nu_longitude > 180
    )
  )


# O resultado esperado é zero registros.

nrow(
  coordenadas_invalidas
)



# ------------------------------------------------------------
# 11.4 VERIFICAR O SINAL DAS COORDENADAS
# ------------------------------------------------------------

# Pernambuco está localizado nos hemisférios Sul e Oeste.
#
# Portanto, para as estações analisadas, espera-se que:
#
# latitude < 0
# longitude < 0
#
# Essa verificação não substitui uma validação espacial em SIG,
# mas permite identificar rapidamente possíveis erros de sinal.

coordenadas_sinal_incomum <- dados_MP10_2017_2025_PE %>%
  filter(
    ano >= 2023,
    !is.na(nu_latitude),
    !is.na(nu_longitude),
    (
      nu_latitude >= 0 |
        nu_longitude >= 0
    )
  )


nrow(
  coordenadas_sinal_incomum
)



# ------------------------------------------------------------
# 11.5 VERIFICAR AS COORDENADAS DISPONÍVEIS POR ESTAÇÃO
# ------------------------------------------------------------

# Nesta etapa são utilizados apenas os registros das bases
# recentes que apresentam informação geográfica.
#
# A função distinct() elimina a repetição de coordenadas
# decorrente das milhares de observações horárias realizadas
# em uma mesma estação.

coordenadas_observadas <- dados_MP10_2017_2025_PE %>%
  filter(
    ano %in% 2023:2025,
    !is.na(nu_latitude),
    !is.na(nu_longitude)
  ) %>%
  transmute(
    ano,
    cod_estacao,
    no_estacao,
    
    # Arredondamento apenas para eliminar diferenças numéricas
    # residuais de representação.
    latitude = round(nu_latitude, 6),
    longitude = round(nu_longitude, 6)
  ) %>%
  distinct()


View(
  coordenadas_observadas
)



# ------------------------------------------------------------
# 11.6 VERIFICAR A ESTABILIDADE DAS COORDENADAS POR ESTAÇÃO
# ------------------------------------------------------------

# Para cada estação são calculados:
#
# número de pares distintos de coordenadas;
# menor e maior latitude;
# menor e maior longitude;
# amplitude observada.
#
# Se uma estação apresentar coordenadas muito diferentes entre
# períodos, a diferença deve ser investigada antes da atribuição
# retrospectiva, pois pode indicar:
#
# - alteração na precisão do cadastro;
# - erro de registro;
# - mudança física da estação;
# - uso do mesmo nome para diferentes pontos de monitoramento.

consistencia_coordenadas <- coordenadas_observadas %>%
  group_by(
    cod_estacao,
    no_estacao
  ) %>%
  summarise(
    
    n_coordenadas_distintas =
      n_distinct(
        paste(latitude, longitude)
      ),
    
    latitude_min =
      min(latitude),
    
    latitude_max =
      max(latitude),
    
    amplitude_latitude =
      latitude_max - latitude_min,
    
    longitude_min =
      min(longitude),
    
    longitude_max =
      max(longitude),
    
    amplitude_longitude =
      longitude_max - longitude_min,
    
    .groups = "drop"
  )


View(
  consistencia_coordenadas
)



# ------------------------------------------------------------
# 11.7 CRIAR UMA COORDENADA DE REFERÊNCIA POR ESTAÇÃO
# ------------------------------------------------------------

# Após a verificação de consistência, é criada uma coordenada
# representativa para cada estação.
#
# A mediana é utilizada como medida de referência porque é
# pouco influenciada por pequenas diferenças de precisão ou por
# registros isolados discrepantes.
#
# Importante:
# a mediana somente deve ser utilizada após verificar a tabela
# "consistencia_coordenadas".
#
# Caso uma estação apresente mudança espacial significativa,
# não se deve atribuir automaticamente uma única coordenada para
# toda a série histórica.

coordenadas_estacoes <- coordenadas_observadas %>%
  group_by(
    cod_estacao,
    no_estacao
  ) %>%
  summarise(
    
    latitude_referencia =
      median(
        latitude,
        na.rm = TRUE
      ),
    
    longitude_referencia =
      median(
        longitude,
        na.rm = TRUE
      ),
    
    .groups = "drop"
  )


View(
  coordenadas_estacoes
)



# ------------------------------------------------------------
# 11.8 ASSOCIAR AS COORDENADAS DE REFERÊNCIA À BASE HISTÓRICA
# ------------------------------------------------------------

# A tabela de referência é associada à base consolidada por meio
# do código e nome normalizados da estação.
#
# Essa associação não modifica inicialmente as coordenadas
# existentes. Apenas adiciona as coordenadas de referência como
# variáveis auxiliares.

dados_MP10_2017_2025_PE <- dados_MP10_2017_2025_PE %>%
  left_join(
    
    coordenadas_estacoes,
    
    by = c(
      "cod_estacao",
      "no_estacao"
    )
  )



# ------------------------------------------------------------
# 11.9 REGISTRAR A ORIGEM DA INFORMAÇÃO GEOGRÁFICA
# ------------------------------------------------------------

# Antes de preencher os valores ausentes, é criada uma variável
# destinada a registrar a procedência das coordenadas.
#
# São utilizadas três classificações:
#
# "Original"
#   coordenada já existente na base MonitorAr;
#
# "Atribuída pela estação"
#   coordenada inexistente na base original, mas preenchida com
#   base na correspondência entre nome/código da estação e sua
#   coordenada de referência;
#
# "Não disponível"
#   estação sem coordenada original e sem referência disponível.
#
# Essa informação é importante para assegurar a rastreabilidade
# metodológica do tratamento realizado.

dados_MP10_2017_2025_PE <- dados_MP10_2017_2025_PE %>%
  mutate(
    
    origem_coordenada = case_when(
      
      !is.na(nu_latitude) &
        !is.na(nu_longitude) ~
        "Original",
      
      ano <= 2022 &
        is.na(nu_latitude) &
        is.na(nu_longitude) &
        !is.na(latitude_referencia) &
        !is.na(longitude_referencia) ~
        "Atribuída pela estação",
      
      TRUE ~
        "Não disponível"
    )
  )



# ------------------------------------------------------------
# 11.10 PREENCHER AS COORDENADAS DE 2017 A 2022
# ------------------------------------------------------------

# As coordenadas de referência são utilizadas somente nos
# registros de 2017 a 2022 em que a latitude ou longitude
# original esteja ausente.
#
# Os valores existentes nas bases MonitorAr são preservados.
#
# Portanto, esta etapa não substitui as coordenadas originais
# de 2023 a 2025.

dados_MP10_2017_2025_PE <- dados_MP10_2017_2025_PE %>%
  mutate(
    
    nu_latitude = case_when(
      
      ano <= 2022 &
        is.na(nu_latitude) ~
        latitude_referencia,
      
      TRUE ~
        nu_latitude
    ),
    
    
    nu_longitude = case_when(
      
      ano <= 2022 &
        is.na(nu_longitude) ~
        longitude_referencia,
      
      TRUE ~
        nu_longitude
    )
  )



# ------------------------------------------------------------
# 11.11 REMOVER AS VARIÁVEIS AUXILIARES
# ------------------------------------------------------------

# Após o preenchimento, as coordenadas de referência utilizadas
# durante o processamento deixam de ser necessárias na tabela
# principal.
#
# A variável "origem_coordenada", entretanto, é preservada para
# documentar se cada coordenada é original ou atribuída.

dados_MP10_2017_2025_PE <- dados_MP10_2017_2025_PE %>%
  select(
    -latitude_referencia,
    -longitude_referencia
  )



# ------------------------------------------------------------
# 11.12 VERIFICAR O RESULTADO FINAL
# ------------------------------------------------------------

# A tabela abaixo permite verificar, por ano e estação, quais
# coordenadas passaram a constar na base e qual foi sua origem.

verificacao_coordenadas <- dados_MP10_2017_2025_PE %>%
  distinct(
    ano,
    cod_estacao,
    no_estacao,
    nu_latitude,
    nu_longitude,
    origem_coordenada
  ) %>%
  arrange(
    no_estacao,
    ano
  )


View(
  verificacao_coordenadas
)



# ------------------------------------------------------------
# 11.13 VERIFICAR EVENTUAIS COORDENADAS AINDA AUSENTES
# ------------------------------------------------------------

dados_MP10_2017_2025_PE %>%
  filter(
    is.na(nu_latitude) |
      is.na(nu_longitude)
  ) %>%
  distinct(
    ano,
    cod_estacao,
    no_estacao
  )

# ------------------------------------------------------------
# 12. SEPARAÇÃO DAS INFORMAÇÕES DE DATA E HORA
# ------------------------------------------------------------

# A variável dh_medicao contém conjuntamente a data e a hora
# da medição.
#
# Para permitir análises em diferentes escalas temporais,
# são criadas duas variáveis adicionais:
#
# data
#   contém exclusivamente a data da observação.
#
# hora
#   contém exclusivamente o horário registrado.
#
# A variável original dh_medicao é preservada para manter
# a rastreabilidade temporal dos dados.

dados_MP10_2017_2025_PE <- dados_MP10_2017_2025_PE %>%
  mutate(
    
    data = as.Date(
      substr(
        dh_medicao,
        1,
        10
      )
    ),
    
    
    hora = substr(
      dh_medicao,
      12,
      19
    )
  )



# ------------------------------------------------------------
# 13. PADRONIZAÇÃO DOS HORÁRIOS DE MEDIÇÃO
# ------------------------------------------------------------

# Durante a inspeção dos dados foram identificadas pequenas
# diferenças na representação temporal de algumas observações.
#
# Além das horas registradas exatamente na hora cheia:
#
# 09:00:00
# 10:00:00
# 11:00:00
#
# também foram encontrados horários como:
#
# 09:59:59
# 10:59:59
#
# e:
#
# 18:00:01
# 19:00:01
#
# Essas diferenças correspondem a apenas um segundo em relação
# à hora cheia, mas poderiam fazer com que o R interpretasse
# essas observações como horários diferentes.
#
# Isso produziria categorias artificiais nas análises horárias.
#
# Para evitar esse problema, a informação temporal é arredondada
# para a hora cheia mais próxima.
#
# A transformação é realizada sobre data e hora conjuntamente.
#
# Essa decisão é necessária porque um horário como:
#
# 2021-05-10 23:59:59
#
# deve resultar em:
#
# 2021-05-11 00:00:00
#
# Ou seja, o arredondamento pode modificar simultaneamente a
# hora e a data.
#
# A informação original NÃO é substituída.
#
# São mantidas:
#
# dh_medicao
# data
# hora
#
# e criadas:
#
# dh_arredondado
# data_fechada
# hora_fechada
#
# Isso permite rastrear a transformação realizada.


dados_MP10_2017_2025_PE <- dados_MP10_2017_2025_PE %>%
  mutate(
    
    # Converter a variável de data/hora em POSIXct.
    #
    # %OS permite interpretar segundos e, quando existentes,
    # frações de segundo, como ".000".
    
    dh_arredondado = as.POSIXct(
      
      round(
        
        as.numeric(
          
          as.POSIXct(
            dh_medicao,
            format = "%Y-%m-%d %H:%M:%OS",
            tz = "America/Recife"
          )
          
        ) / 3600
        
      ) * 3600,
      
      origin = "1970-01-01",
      tz = "America/Recife"
    ),
    
    
    # Data correspondente ao horário já padronizado.
    
    data_fechada = as.Date(
      dh_arredondado,
      tz = "America/Recife"
    ),
    
    
    # Hora completa resultante do arredondamento.
    
    hora_fechada = format(
      dh_arredondado,
      "%H:%M:%S",
      tz = "America/Recife"
    )
  )



# ------------------------------------------------------------
# 14. CRIAÇÃO DE VARIÁVEIS TEMPORAIS AUXILIARES
# ------------------------------------------------------------

# Para permitir análises mensais e facilitar agrupamentos
# estatísticos, são criadas duas variáveis adicionais:
#
# mes
#   número correspondente ao mês, variando de 1 a 12;
#
# mes_nome
#   identificação textual abreviada do mês.
#
# A variável é derivada de data_fechada, ou seja, utiliza
# a informação temporal já corrigida pelo arredondamento.

dados_MP10_2017_2025_PE <- dados_MP10_2017_2025_PE %>%
  mutate(
    
    mes = as.integer(
      format(
        data_fechada,
        "%m"
      )
    ),
    
    
    mes_nome = factor(
      
      mes,
      
      levels = 1:12,
      
      labels = c(
        "Jan",
        "Fev",
        "Mar",
        "Abr",
        "Mai",
        "Jun",
        "Jul",
        "Ago",
        "Set",
        "Out",
        "Nov",
        "Dez"
      )
    )
  )



# ------------------------------------------------------------
# 15. VERIFICAÇÕES DE CONSISTÊNCIA DA BASE CONSOLIDADA
# ------------------------------------------------------------

# Após a integração e padronização são realizadas verificações
# destinadas a identificar possíveis inconsistências e confirmar
# que as transformações produziram o resultado esperado.



# ------------------------------------------------------------
# 15.1 QUANTIDADE DE REGISTROS POR ANO
# ------------------------------------------------------------

# Permite verificar:
#
# - quais anos estão efetivamente representados;
# - a quantidade de observações disponível em cada período;
# - anos com ausência completa de registros;
# - diferenças importantes de disponibilidade temporal.

table(
  dados_MP10_2017_2025_PE$ano
)



# ------------------------------------------------------------
# 15.2 QUANTIDADE DE REGISTROS POR ANO E ESTAÇÃO
# ------------------------------------------------------------

# Essa análise permite avaliar inicialmente a distribuição dos
# dados entre as estações e identificar períodos em que
# determinada estação não apresenta observações.

dados_MP10_2017_2025_PE %>%
  count(
    ano,
    cod_estacao,
    no_estacao
  ) %>%
  arrange(
    ano,
    cod_estacao
  )



# ------------------------------------------------------------
# 15.3 CORRESPONDÊNCIA ENTRE CÓDIGO E NOME DA ESTAÇÃO
# ------------------------------------------------------------

# Após a normalização, espera-se que cada estação apresente
# apenas uma combinação entre código e nome.

dados_MP10_2017_2025_PE %>%
  distinct(
    cod_estacao,
    no_estacao
  ) %>%
  arrange(
    cod_estacao
  )



# ------------------------------------------------------------
# 15.4 VERIFICAÇÃO DA ESTRUTURA DA BASE FINAL
# ------------------------------------------------------------

# Exibe todas as variáveis presentes após o processamento.

names(
  dados_MP10_2017_2025_PE
)



# ------------------------------------------------------------
# 15.5 INSPEÇÃO DOS HORÁRIOS ORIGINAIS
# ------------------------------------------------------------

# Permite visualizar todos os padrões de horário existentes antes
# do arredondamento.

sort(
  unique(
    dados_MP10_2017_2025_PE$hora
  )
)



# ------------------------------------------------------------
# 15.6 INSPEÇÃO DOS HORÁRIOS PADRONIZADOS
# ------------------------------------------------------------

# O resultado esperado é a existência apenas das 24 horas
# completas do dia:
#
# 00:00:00
# 01:00:00
# ...
# 23:00:00

sort(
  unique(
    dados_MP10_2017_2025_PE$hora_fechada
  )
)



# ------------------------------------------------------------
# 15.7 COMPARAÇÃO ENTRE HORÁRIO ORIGINAL E PADRONIZADO
# ------------------------------------------------------------

# Permite identificar diretamente quais horários sofreram
# alteração durante o arredondamento.

dados_MP10_2017_2025_PE %>%
  distinct(
    hora,
    hora_fechada
  ) %>%
  arrange(
    hora
  )



# ------------------------------------------------------------
# 15.8 VERIFICAÇÃO DA COBERTURA TEMPORAL
# ------------------------------------------------------------

# A quantidade total de registros não informa, isoladamente,
# se os dados estão distribuídos ao longo de todo o ano.
#
# Por esse motivo, é criada uma tabela contendo a primeira e
# a última data disponível para cada estação em cada ano.
#
# Essa verificação será importante posteriormente para avaliar
# a completude temporal das séries e definir quais períodos
# podem ser utilizados como referência para médias anuais.


cobertura_temporal <- dados_MP10_2017_2025_PE %>%
  group_by(
    ano,
    cod_estacao,
    no_estacao
  ) %>%
  summarise(
    
    primeira_medicao = min(
      data_fechada,
      na.rm = TRUE
    ),
    
    ultima_medicao = max(
      data_fechada,
      na.rm = TRUE
    ),
    
    n_registros = n(),
    
    .groups = "drop"
  )


View(
  cobertura_temporal
)



# ------------------------------------------------------------
# 16. EXPORTAÇÃO DA BASE CONSOLIDADA
# ------------------------------------------------------------

# A exportação é realizada somente após a criação das variáveis
# derivadas e a padronização das informações.
#
# São produzidos dois formatos:
#
# CSV:
# destinado à interoperabilidade com outros programas, inspeção
# externa e eventual compartilhamento dos dados.
#
# RDS:
# formato nativo do R, utilizado como arquivo principal de
# trabalho porque preserva classes e tipos das variáveis, como
# Date, POSIXct e factor.



# ------------------------------------------------------------
# 16.1 SALVAR EM CSV
# ------------------------------------------------------------

write.csv(
  
  dados_MP10_2017_2025_PE,
  
  "dados_MP10_2017_2025_PE_v1.csv",
  
  row.names = FALSE,
  
  fileEncoding = "UTF-8"
)



# ------------------------------------------------------------
# 16.2 SALVAR EM RDS
# ------------------------------------------------------------

saveRDS(
  
  dados_MP10_2017_2025_PE,
  
  "dados_MP10_2017_2025_PE_v1.rds"
)



# ------------------------------------------------------------
# 16.3 CONFIRMAR A CRIAÇÃO DOS ARQUIVOS
# ------------------------------------------------------------

file.exists(
  "dados_MP10_2017_2025_PE_v1.csv"
)

file.exists(
  "dados_MP10_2017_2025_PE_v1.rds"
)

media_anual_geral <- dados_MP10_2017_2025_PE %>%
  group_by(ano) %>%
  summarise(
    
    # Total de registros no ano
    n_total = n(),
    
    # Registros com concentração informada
    n_validos = sum(!is.na(nu_concentracao)),
    
    # Quantidade de valores negativos
    n_negativos = sum(
      nu_concentracao < 0,
      na.rm = TRUE
    ),
    
    # Quantidade utilizada no cálculo da média
    n_utilizados = sum(
      nu_concentracao >= 0,
      na.rm = TRUE
    ),
    
    # Média anual de MP10 utilizando apenas valores >= 0
    media_anual_MP10 = mean(
      nu_concentracao[nu_concentracao >= 0],
      na.rm = TRUE
    ),
    
    .groups = "drop"
  )

media_anual_estacao <- dados_MP10_2017_2025_PE %>%
  group_by(
    ano,
    cod_estacao,
    no_estacao
  ) %>%
  summarise(
    
    # Total de registros da estação no ano
    n_total = n(),
    
    # Registros com concentração informada
    n_validos = sum(
      !is.na(nu_concentracao)
    ),
    
    # Valores negativos
    n_negativos = sum(
      nu_concentracao < 0,
      na.rm = TRUE
    ),
    
    # Quantidade utilizada no cálculo
    n_utilizados = sum(
      nu_concentracao >= 0,
      na.rm = TRUE
    ),
    
    # Média anual da estação
    media_anual_MP10 = mean(
      nu_concentracao[nu_concentracao >= 0],
      na.rm = TRUE
    ),
    
    .groups = "drop"
  ) %>%
  arrange(
    ano,
    cod_estacao
  )