# Consolidação metodológica da Etapa 9

## Fontes

Temperatura do ar, umidade relativa, velocidade e direção do vento e pressão atmosférica são provenientes exclusivamente de estações automáticas do INMET. A precipitação é proveniente da rede CEMADEN, conforme os pareamentos avaliados nas subetapas 9D a 9H.

## Regras de validade

Para as variáveis horárias do INMET, adotou-se operacionalmente como critério principal de agregação diária a disponibilidade de pelo menos 18 horas válidas em 24 horas (75%). Os limiares de 16 e 20 horas são preservados como análises de sensibilidade. A validade é específica para cada variável, de modo que a indisponibilidade de uma variável não invalida automaticamente as demais.

A direção do vento é tratada como variável circular. Foram preservadas a direção média circular e as componentes vetoriais u e v, evitando a média aritmética simples dos ângulos.

Para a precipitação CEMADEN, a regra principal utiliza continuidade diária com maior intervalo entre registros de até 60 minutos, mantendo 70 e 90 minutos como sensibilidades. Registros exatamente à 00:00 local são atribuídos ao dia civil anterior, conforme a convenção temporal congelada na Etapa 9F.

## Integração

Não foi realizada imputação de valores ausentes, preenchimento entre estações ou construção de séries híbridas. As séries de sensibilidade são mantidas independentes das séries principais.

## Escopo

A base integrada desta etapa corresponde aos station-years utilizados como referências anuais válidas de MP10 e que possuem pareamento meteorológico fechado. SUAPE 2022 permanece fora da base integrada até a obtenção de coordenada histórica documentalmente válida da estação PE06.

## Uso analítico

A base diária principal integra MP10, variáveis meteorológicas do INMET e precipitação CEMADEN para as análises subsequentes. As limitações de distância, cobertura temporal e disponibilidade por variável devem ser consideradas na interpretação das associações, especialmente para vento em Gaibu 2019, para a interrupção da A301 em 2021 e para umidade relativa em IPOJUCA 2025.
