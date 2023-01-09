# Bike Sharing Report
 Relatório de Análise de Dados  e resolução de problema de negócio com base em dados de empresa de aluguel de bicicletas.

## Sumário
1. Introdução
2. Limpeza e Transformação
3. Análise
4. Conclusão

## 1. Introdução

### Contexto

Este relatório foi realizado conforme as orientações do curso do Certificado
Profissional de Análise de Dados do Google, de acordo com as orientações e
problema de negócio fictício disponíveis nesse [documento](https://github.com/lucas-a-correa/Bike-Sharing-Report/blob/main/Bike_Sharing_Business_Problem.pdf).
  
#### Dados
  
O dados utilizados para a análise estão nesse [bucket](https://divvy-tripdata.s3.amazonaws.com/index.html)
público da empresa [Divvy](https://divvybikes.com), referentes a todas as viagens realizadas no ano de 2022.

#### Problema de Negócio

No cenário, a empresa **_Cyclistic_**, empresa fictícia baseada na empresa fornecedora dos dados,
é uma empresa de aluguel de bicicletas na cidade de Chicago, Illinois. A empresa
possui um modelo de negócio em que o cliente pode contratar uma assinatura anual e
utilizar as bicletas quantas vezes quiser, pagar uma taxa diária ou pagar uma taxa única, 
baseada na quantidade de minutos utilizados.

O departamento financeiro da empresa concluiu que os assinantes anuais são mais lucrativos
para a empresa e, por isso, o diretor de marketing deseja lançar uma campanha direcionada
aos usuários não-assinantes, chamados de "Casuais", visando transformá-los em assinantes.

Para apoiar a tomada de decisão sobre a campanha de marketing, o time de análise de dados
deve analisar os dados das viagens coletados pela empresa para melhor entender os perfis dos clientes
e fundamentar as decisões da direção.

## 2. Limpeza e Tranformação

Os dados fornecidos pela empresa são fornecidos em arquivos CSV, divididos por mês.
A limpeza e transformação dos dados foi executada utilizando a linguagem R. O
arquivo completo pode ser visualizado [aqui](https://github.com/lucas-a-correa/Bike-Sharing-Report/blob/main/Scripts/Bike_Cleaning.R).

Os seguintes problemas foram encontrados nos dados:
1. Viagens com informações sobre as estações de início e fim ausentes;
2. Viagens com coordenadas entre estações discrepantes, com algumas viagens apresentando distâncias
com milhares de quilômetros;
3. Viagens com informações sobre a duração da viagem discrepantes, com algumas viagens
apresentando duração de dezenas de horas.
Para excluir os dados resultantes de dados incorretos foram excluídas as viagens
com distâncias superiores a 20km e duração superiores a 3h.

Os dados iniciais possuem as seguintes informações:

Id da viagem, tipo de bicicleta, horário de início e fim, nome e id da estação de início e fim,
coordenadas em grau decimal do início e fim da viagem e se o usuário era assinante ou casual.

A partir desses dados podemos extrair as seguintes informações para apoiar nossa análise:

Duração da viagem, Distância da viagem, mês e dia da semana.

```R
#transforma as strings de tempo em datetime, extrai o mês,
#calcula o tempo da viagem, extrai o dia da semana
extrair_duracao <- function(df){
  df <- df %>% 
    mutate(
      started_at_clean = ymd_hms(started_at)
    ) %>% 
    mutate(
      ended_at_clean = ymd_hms(ended_at)
    ) %>% 
    mutate(
      period = case_when(
        ((hour(started_at)>=6)&(hour(started_at)<12)) ~"Morning",
        ((hour(started_at)>=12)&(hour(started_at)<18)) ~"Afternoon",
        ((hour(started_at)>=18)&(hour(started_at)<=23)) ~"Night",
        ((hour(started_at)>=0)&(hour(started_at)<6)) ~"Dawn",
      )
    ) %>% 
    mutate(
      month = month(started_at)
    ) %>% 
    mutate(
      trip_duration = as.integer(ended_at_clean - started_at_clean)
    ) %>% 
    rowwise() %>% 
    mutate(
      weekday = as.character(wday(started_at_clean))
    )
  bike_2022_df <- bike_2022_df %>% 
    rowwise() %>% 
    mutate(
      weekday = case_when(
        weekday == '1' ~'Sunday',
        weekday == '2' ~'Monday',
        weekday == '3' ~'Tuesday',
        weekday == '4' ~'Wednesday',
        weekday == '5' ~'Thursday',
        weekday == '6' ~'Friday',
        weekday == '7' ~'Saturday',
      )
    )
  return(df)
}

#Função para extrair a distância das viagens
extrair_distancia <- function(df){
  new_df <- df %>% 
    rowwise() %>% 
    mutate(
      trip_distance = haversine(
        c(start_lat,start_lng),c(end_lat,end_lng)
      )
    )
  return(new_df)
}
```
## 3. Análise

![Chart](/Charts/Members.PNG)

Nossa amostra final é de pouco mais de 5,6 milhões de viagens, com 59,39% desse número
composto por membros assinantes e 40,61% de usuários casuais. O maior número de usuários membros pode ter relação com a preferência e lealdade à marca. Um membro assinante tem uma menor probabilidade de utilizar outras marcas como substitutas, planejando sua utilização de acordo com a disponibilidade de estações, enquanto os usuários casuais possuem maior facilidade em utilizar outra marca a depender da disponibilidade.

![Chart](/Charts/Anual_Dist.PNG)

Podemos observar que a utilização do serviço durante o ano é parecida nos dois grupos, começando o ano com valores baixos, de menos de 25 mil utilizações por dia, aumentando enquanto caminha para o meio do ano, atingindo um pico nos meses de julho e agosto, ultrapassando as 150 mil utilizações por dia, e caindo novamente nos meses de agosto e outubro. Devemos lembrar que a empresa analisada é situada na cidade de Chicago, que atinge temperaturas abaixo de 0°C no inverno (que ocorre de dezembro a março no hemisfério norte), o que influencia na utilização de bicicletas.
Também podemos observar que, embora também apresente um aumento nos meses de verão, a variação da utilização dos membros é menos acentuada, o que sugere uma maior constância na utilização.

![Chart](/Charts/Period_Dist.PNG)

