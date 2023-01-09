#carrega os pacotes necessários
library(tidyverse)
library(lubridate)
library(skimr)
library(pracma)

#carrega os arquivos CSV em um DataFrame
bike_data_jan <- read.csv("202201-divvy-tripdata.csv")
bike_data_fev <- read.csv("202202-divvy-tripdata.csv")
bike_data_mar <- read.csv("202203-divvy-tripdata.csv")
bike_data_abr <- read.csv("202204-divvy-tripdata.csv")
bike_data_mai <- read.csv("202205-divvy-tripdata.csv")
bike_data_jun <- read.csv("202206-divvy-tripdata.csv")
bike_data_jul <- read.csv("202207-divvy-tripdata.csv")
bike_data_ago <- read.csv("202208-divvy-tripdata.csv")
bike_data_set <- read.csv("202209-divvy-tripdata.csv")
bike_data_out <- read.csv("202210-divvy-tripdata.csv")
bike_data_nov <- read.csv("202211-divvy-tripdata.csv")
bike_data_dez <- read.csv("202212-divvy-tripdata.csv")

#lista os arquivos para aplicar os loops
df_list <- list(bike_data_jan,bike_data_fev,bike_data_mar,bike_data_abr,
                bike_data_mai,bike_data_jun,bike_data_jul,bike_data_ago,
                bike_data_set,bike_data_out,bike_data_nov,bike_data_dez)

#deleta colunas que não nos interessam
deletar_colunas <- function(df){
  df <- df %>% 
    select(-c(
      ride_id,start_station_id,end_station_id,started_at,ended_at
    ))
  return(df)
}

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
    )%>% 
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

#Função para excluir outliers
excluir_outliers <- function(df){
  df <- subset(df,(df$trip_distance < 20))
  df <- subset(df, (df$trip_duration < 7200))
  return(df)
}

df_list <- lapply(df_list, drop_na)

df_list <- lapply(df_list, extrair_duracao)

df_list <- lapply(df_list, extrair_distancia)

df_list <- lapply(df_list, excluir_outliers)

df_list <- lapply(df_list, deletar_colunas)

#concatena os dataframes
bike_2022_df <- data.frame()
for(x in 1:12){
  bike_2022_df <- rbind(bike_2022_df,df_list[[x]])
}

#Exclui as linhas com tempos negativos
bike_2022_df <- bike_2022_df[!(bike_2022_df$trip_duration <= 0),]

#Resumo das métricas de tempo e distância por tipo de usuário e período
bike_tempodist_userperiod <- bike_2022_df %>% 
  group_by(period, member_casual) %>% 
  summarise(
    mean_distance = mean(trip_distance), mean_duration = mean(trip_duration)
    )

#resumo do total de viagens por tipo de usuário e período
bike_totaltrips_userperiod <- bike_2022_df %>% 
  group_by(period, member_casual) %>% 
  summarise(
    total_trips = n()
  )

#Ordena as estações mais utilizadas para iniciar viagens por membros
start_stations_member <- bike_2022_df[bike_2022_df$member_casual == "member",] %>% 
  group_by(start_station_name) %>% 
  summarise(total_trips = n()) %>% 
  arrange(desc(total_trips))

#Ordena as estações mais utilizadas para terminar viagens por membros
end_stations_member <- bike_2022_df[bike_2022_df$member_casual == "member",] %>% 
  group_by(end_station_name) %>% 
  summarise(total_trips = n()) %>% 
  arrange(desc(total_trips))

#Ordena as estações mais utilizadas para iniciar viagens por casuais
start_stations_casual <- bike_2022_df[bike_2022_df$member_casual == "casual",] %>% 
  group_by(start_station_name) %>% 
  summarise(total_trips = n()) %>% 
  arrange(desc(total_trips))

#Ordena as estações mais utilizadas para terminar viagens por casuais
end_stations_casual <- bike_2022_df[bike_2022_df$member_casual == "casual",] %>% 
  group_by(end_station_name) %>% 
  summarise(total_trips = n()) %>% 
  arrange(desc(total_trips))


#resumo do total de viagens por tipo de usuário e dia da semana
bike_totaltrips_weekday <- bike_2022_df %>% 
  group_by(weekday, member_casual) %>% 
  summarise(
    total_trips = n()
  )
wday_reference = c(
  'Sunday','Monday','Tuesday','Wednesday','Thursday','Friday','Saturday'
  )
bike_totaltrips_weekday <- bike_totaltrips_weekday[
  order(factor(bike_totaltrips_weekday$weekday, levels=wday_reference)),
  ]


