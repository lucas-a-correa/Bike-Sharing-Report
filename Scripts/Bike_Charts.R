library(tidyverse)

#Gera e salva o gráfico de viagens por tipo de usuário
df <- count(bike_2022_df, member_casual)

ggplot(df)+
  geom_col(mapping = aes(x=member_casual,y=n,fill=member_casual))+
  scale_fill_brewer(palette = 'Accent')+
  labs(x='',y='Viagens',title = "Total de Viagens por Tipo de Usuário",
       fill="Tipo de Usuário")+
  annotate("text", x=2, y=3000000,
           label="3,3 mi\n (59,39%)",size=5)+
  annotate("text", x=1, y=2000000, label="2,2 mi\n (40,61%)",size=5)

ggsave("Members.png")

#Gera e salva o gráfico de distância por tipo de usuário
df2 <- data.frame(member_casual = c('total'),
                  Mean_Time = c(as.double(mean(bike_2022_df$trip_duration))),
                  Mean_Distance = c(as.double(mean(bike_2022_df$trip_distance)))
                  )

df <- bike_2022_df %>% 
  group_by(member_casual) %>% 
  summarise(Mean_Time = mean(trip_duration),
                             Mean_Distance = mean(trip_distance))

df<- bind_rows(df, df2)

ggplot(bike_2022_df)+
  geom_histogram(mapping = aes(x=trip_distance, fill=member_casual))+
  scale_fill_brewer(palette = 'Accent')+
  labs(x='Distância (km)',y='Viagens',title = "Distância por Tipo de Usuário",
       fill="Tipo de Usuário")+
  annotate("text", x=7, y=1250000,
           label="Média Casuais: 2,17km",size=5)+
  annotate("text", x=7, y=600000, label="Média Membros: 2,09km",size=5)

ggsave("Km_Dist.png")

#Gera e salva o gráfico de tempo por tipo de usuário
ggplot(bike_2022_df)+
  geom_histogram(mapping = aes(x=trip_duration, fill=member_casual))+
  scale_fill_brewer(palette = 'Accent')+
  labs(x='Tempo (s)',y='Viagens',title = "Tempo de Viagem por Tipo de Usuário",
       fill="Tipo de Usuário")+
  annotate("text", x=3000, y=1000000,
           label="Média Casuais: 18m51s",size=5)+
  annotate("text", x=3000, y=600000, label="Média Membros: 11m53s",size=5)

ggsave("Time_Dist.png")

#Gera e salva o gráfico de Usuários por Período do dia
df <- data.frame(Percentage = c('45,1%', '42,5%','5,4%','3,6%',
                                '18,8%','27,2%','30,6%','26,7%'))
bike_totaltrips_userperiod <- bind_cols(bike_totaltrips_userperiod,df)

ggplot(
  bike_totaltrips_userperiod, aes(
    x=member_casual, y=total_trips,group=period
  )
)+
  geom_col(aes(fill=period))+
  geom_text(
    aes(label=Percentage...5), position=position_stack(vjust = 0.5)
  )+
  scale_fill_brewer(palette = 'Accent')+
  labs(
    x='',y='Viagens',title = "Usuários por Período do dia",fill="Períodos"
  )

ggsave("Period_Dist.png")

#Gera e salva o gráfico de Viagens por Dia
ggplot(bike_2022_df)+
  geom_freqpoly(mapping = aes(x=started_at_clean, color=member_casual))+
  scale_fill_brewer(palette = 'Accent')+
  labs(x='',y='Viagens',title = "Viagens por Dia",
       fill="Tipos de Usuários")

ggsave("Anual_Dist.png")

order <- c(
  'Monday','Tuesday','Wednesday','Thursday','Friday','Saturday', "Sunday"
)
ggplot(bike_totaltrips_weekday)+
  geom_col(
    mapping = aes(
      x=weekday,y=total_trips, fill=member_casual
    ), position="dodge"
  )+
  scale_fill_brewer(palette = 'Accent')+
  scale_x_discrete(limits=order)+
  labs(x='Dias da Semana',y='Viagens',title = "Viagens por Dia da Semana",
       fill="Tipos de Usuários")

ggsave("WDay_Users.PNG")

bike_data_wday_means <- bike_2022_df %>% 
  group_by(weekday,period,member_casual) %>% 
  summarise(Avg_Time = mean(trip_duration),Avg_Dist = mean(trip_distance))
bike_data_wday_means <- bike_data_wday_means[
  order(factor(bike_data_wday_means$weekday, levels=order)),
]

ggplot(bike_data_wday_means)+
  geom_col(
    mapping=aes(
      x=weekday,y=Avg_Time, fill=member_casual
    ), position="dodge"
  )+
  scale_fill_brewer(palette = 'Accent')+
  facet_wrap(~member_casual)+
  scale_x_discrete(limits=order)+
  labs(x='Dias da Semana',y='Tempo (s)',title = "Tempo Médio por Dia da Semana",
       fill="Tipos de Usuários")

ggsave("WDay_Time.PNG")

ggplot(bike_data_wday_means)+
  geom_col(
    mapping=aes(
      x=weekday,y=Avg_Dist, fill=member_casual
    ), position="dodge"
  )+
  scale_fill_brewer(palette = 'Accent')+
  facet_wrap(~member_casual)+
  scale_x_discrete(limits=order)+
  labs(x='Dias da Semana',y='Distância (Km)',
       title = "Distância Média por Dia da Semana",
       fill="Tipos de Usuários")

ggsave("WDay_Km.PNG")

ggplot(bike_2022_df)+
  geom_bar(
    mapping = aes(
      x=weekday, fill=member_casual
    ), position="dodge"
  )+
  facet_wrap(~period)+
  scale_fill_brewer(palette = 'Accent')+
  scale_x_discrete(limits=order)+
  labs(x='Dias da Semana',y='Viagens',
       title = "Usuários por período por Dia da Semana",
       fill="Tipos de Usuários")

ggsave("WDay_Period.PNG")