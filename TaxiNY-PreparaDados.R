library(plotly)
library(stringr)
library(lubridate)

###########################################
##      Total de corridas por mes        ##
###########################################

#calcula o total de corridas por mes e dia da semana, depois aloca em um novo data frame
corridas.taxis.ny.enriq %>% select(mes_inicio) %>%
  group_by(mes_inicio) %>% summarise(total_viagens = n()) -> viagens_por_mes

#adiciona uma nova coluna com os nomes dos meses
viagens_por_mes[order(viagens_por_mes$mes_inicio),] %>% 
  mutate(mes=c('Janeiro', 'Fevereiro', 'Marco', 'Abril', 'Maio', 'Junho')) -> viagens_por_mes

#Indica que o mes deve ser ordenado pelo 'indice' e não por ordem alfabética
viagens_por_mes$mes <- factor(viagens_por_mes$mes, levels = viagens_por_mes[["mes"]])

##################################################
##      Total de corridas por dia da semana     ##
##################################################

#calcula o total de corridas por dia da semana e aloca em um novo data frame
corridas.taxis.ny.enriq %>% select(dia_semana_inicio) %>%
  group_by(dia_semana_inicio) %>% summarise(total_viagens = n()) -> viagens_por_dia_semana

#adiciona uma nova coluna com os nomes dos dias
viagens_por_dia_semana[order(viagens_por_dia_semana$dia_semana_inicio),] %>% 
  mutate(dia=c('Domingo', 'Segunda', 'Terca', 'Quarta', 'Quinta', 'Sexta', 'Sabado')) -> viagens_por_dia_semana

#Indica que o dia da semana deve ser ordenado pelo 'indice' e não por ordem alfabética
viagens_por_dia_semana$dia <- factor(viagens_por_dia_semana$dia, levels = viagens_por_dia_semana[["dia"]])

#Total de Corridas por quarto de hora


####################################################
##      Total de corridas por quarto de hora      ##
####################################################

library(dplyr)
library(stringr)
library(plotly)
library(lubridate)

# cria um novo data frame com o total de corridas por dia da semana, hora e quarto de hora
corridas.taxis.ny.enriq %>% select(pickup_datetime, dia_semana_inicio, hora_inicio) %>% #filtra apenas os dados necesarios
  mutate(min_inicio=minute(ymd_hms(pickup_datetime))) %>% #adiciona uma coluna com o minuto de inicio da corrida
  group_by(dia_semana_inicio, hora_inicio, quarto_hora=cut(min_inicio, c(-1,14,29,44,59), c(15,30,45,60))) %>% # agrupa por dia, hora e quarto de hora
  summarise(total_viagens = n()) -> viagens_dia_semana_quarto_hora # calcula to total e adiciona no novo data frame
# no cut acima foi utilizado o -1 no primeiro grupo para incluir o minuto 0 (zero). Os minutos foram agrupados da seguinte forma:
# Minuto 0 a 14 -> grupo 15
# Minuto 15 a 29 -> grupo 30
# Minuto 30 a 44 -> grupo 45
# Minuto 45 a 59 -> grupo 60

# ordena os dados por dia da semana, hora e quarto de hora
viagens_dia_semana_quarto_hora <- 
  viagens_dia_semana_quarto_hora[order(viagens_dia_semana_quarto_hora$dia_semana_inicio,
                                       viagens_dia_semana_quarto_hora$hora_inicio, 
                                       viagens_dia_semana_quarto_hora$quarto_hora),]

# cria um novo campo ao data frame concatenando a hora com o quarto de hora
viagens_dia_semana_quarto_hora$horario <- paste(str_pad(viagens_dia_semana_quarto_hora$hora_inicio, 2, pad = "0"), 
                                                ":", 
                                                viagens_dia_semana_quarto_hora$quarto_hora, sep = "")

# cria um novo data frame somando todas as viagens 
viagens_dia_semana_quarto_hora %>% select(horario, total_viagens) %>% group_by(horario) %>% 
  summarise(total=sum(total_viagens)) -> viagens_quarto_hora


library(ggplot2)
library(ggmap)


############################################################
############################################################
### EXIBE AS SAÍDAS E CHEGADAS DAS VIAGENS DE TAXI NO    ###  
### MAPA DE NOVA YORK.                                   ###
############################################################

# returns string w/o leading or trailing whitespace
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

#Seleciona o data frame que será utilizado para gerar o mapa 
df <- corridas.taxis.ny.enriq
#Cria o data frame com as saídas dos taxis 
df_ori <- data.frame(lat=df$pickup_latitude,long=df$pickup_longitude)
#Cria o data frame com as chegadas dos taxis
df_dest <- data.frame(lat=df$dropoff_latitude,long=df$dropoff_longitude)

#demarca a área das saídas
box <- make_bbox(long, lat, data = df_ori)

#Gera o mapa das saídas 
map_saidas <-
  ggmap(
    ggmap::get_map(location = box, maptype="terrain", source="google")
  ) +
  geom_point(data=df_ori, x=df_ori$long, y=df_ori$lat, color="red") +
  ggtitle("Saídas de taxi") +
  theme(plot.title = element_text(color="black", face="bold", size=16, hjust=0))



#Grava os dados do mapa
setwd(diretorio_imagens)
nome_arq_png_saidas <-  "saidas_taxi.png"
nome_arq_png_saidas_completo <- paste(trim(diretorio_imagens),.Platform$file.sep,"saidas_taxi.png",sep="")
arq<-png(nome_arq_png_saidas)
print(map_saidas)
ret <- dev.off()

#demarca a área das chegadas 
box <- make_bbox(long, lat, data = df_dest)

#Gera o mapa das chegadas 
map_chegadas <-
  ggmap(
    ggmap::get_map(location = box, maptype="terrain", source="google")
  ) +
  geom_point(data=df_dest, x=df_dest$long, y=df_dest$lat, color="blue")+
  ggtitle("Chegadas de taxi") +
  theme(plot.title = element_text(color="black", face="bold", size=16, hjust=0))



#Grava os dados do mapa
setwd(diretorio_imagens)
nome_arq_png_chegadas <- "chegadas_taxi.png"
nome_arq_png_chegadas_completo <- paste(trim(diretorio_imagens),.Platform$file.sep ,"chegadas_taxi.png",sep="")
arq<-png(nome_arq_png_chegadas)
print(map_chegadas)
ret <- dev.off()



#Pontos de referencia para analise


##########################################################
##                 Pontos de Referencia                 ##
##########################################################
pontos_interesse <- matrix( c(
  c("Empire State Building"              , 40.74844, -73.98566),
  c("Times Square"                       , 40.75890, -73.98513),
  c("High Line"                          , 40.74799, -74.00476),
  c("Metropolitan Museum of Art (MET)"   , 40.77944, -73.96324),
  c("Rockfeller Center"                  , 40.75874, -73.97867),
  c("The Museum of Modern Art(MoMA)"     , 40.76143, -73.97762),
  c("Estacao Grand Central"              , 40.75273, -73.97723),
  c("One World Trade Center"             , 40.71274, -74.01338),
  c("Memorial 11 Setembro"               , 40.71158, -74.01328),
  c("SoHo"                               , 40.72330, -74.00299),
  c("Chinatown"                          , 40.71575, -73.99703),
  c("Museum of Natural History"          , 40.78132, -73.97399),
  c("Parque Bryant"                      , 40.75360, -73.98323),
  c("Intrepid Sea, Air & Space Museum"   , 40.76453, -73.99961),
  c("Madame Tussauds (Museu de Cera)"    , 40.75643, -73.98883),
  c("Organizacao das Nacoes Unidas (ONU)", 40.74888, -73.96801),
  c("Chelsea Market"                     , 40.74244, -74.00614),
  c("Touro de Wall Street"               , 40.70555, -74.01344),
  c("Battery Park"                       , 40.70328, -74.01703),
  c("Zoologico do Central Park"          , 40.76778, -73.97183)
), ncol=3,  byrow=TRUE)
colnames(pontos_interesse) <- c("Ponto", "Latitude", "Longitude")

pontos_interesse <- as.data.frame(pontos_interesse)

#calcula a posicao na matriz
pontos_interesse %>% mutate(posicao_coluna_matriz=calc_pos_coluna(as.numeric(as.character(Longitude)),lista_ny)) %>%
  mutate(posicao_linha_matriz=calc_pos_linha(as.numeric(as.character(Latitude)),lista_ny)) -> pontos_interesse

#Adiciona novas colunas de latitude e longitude transformando em numerico 
pontos_interesse %>% mutate(lon=as.numeric(as.character(Longitude))) %>%
  mutate(lat=as.numeric(as.character(Latitude))) -> pontos_interesse

#Cria o data frame com as saídas dos taxis 
df_ponto <- data.frame(lat=as.numeric(as.character(pontos_interesse$Latitude)),
                       long=as.numeric(as.character(pontos_interesse$Longitude)))

#Gera nomes apenas para identificar pelo número
#utiliza-se a padrão: P-#, onde # é o número do ponto dentro do data.frame de pontos de interesse
df_ponto$nome <- ""

for (i in 1:nrow(df_ponto)){
  df_ponto[i,"nome"] <- paste("P-",i,sep="")  
}




#Gera o mapa das saídas 
map_pontos_interesse <-
  ggmap(
    ggmap::get_map(location = c(lon=mean(df_ponto$long),lat=mean(df_ponto$lat)), maptype="terrain", source="google",zoom=13,scale=2)
  ) +
  geom_point(data=df_ponto, x=df_ponto$long, y=df_ponto$lat, color="purple",size=2) +
  geom_text(aes(x=df_ponto$long,y=df_ponto$lat,label=df_ponto$nome),
            data=pontos_interesse,vjust=-0.5,size=3)+
  ggtitle("Pontos de interesse") +
  theme(plot.title = element_text(color="black", face="bold", size=16, hjust=0))



#Grava os dados do mapa
setwd(diretorio_imagens)
nome_arq_png_pontos <-  "pontos_interesse.png"
nome_arq_png_pontos_completo <- paste(trim(diretorio_imagens),.Platform$file.sep,"pontos_interesse.png",sep="")
arq<-png(nome_arq_png_pontos)
print(map_pontos_interesse)
ret <- dev.off()


#Analise Exploratória Inicial

library("magrittr")
library("dplyr")
library("lubridate")

#Dia do ano conforme inicio da corrida
corridas.taxis.ny.enriq %>% mutate(dia_ano_inicio = strftime(as.Date(pickup_datetime), format = "%j")) -> corridas.taxis.ny.enriq

#Os principais horários das corridas
#Distinção por dia da semana; E por dia do ano; Por hora do dia;
corridas.taxis.ny.enriq %>% select(dia_semana_inicio) %>% group_by(dia_semana_inicio) %>% summarise('total' = n()) %>% arrange(desc(total)) -> principais_dias_semana


corridas.taxis.ny.enriq %>% select(dia_ano_inicio) %>% group_by(dia_ano_inicio) %>% summarise('total' = n()) %>% arrange(desc(total)) -> principais_dias_ano
corridas.taxis.ny.enriq %>% select(dia_mes_inicio, mes_inicio) %>% group_by(dia_mes_inicio, mes_inicio) %>% summarise('total' = n()) %>% arrange(desc(total)) -> principais_dias_ano

corridas.taxis.ny.enriq %>% select(hora_inicio) %>% group_by(hora_inicio) %>% summarise('total' = n()) %>% arrange(desc(total)) -> principais_horarios

#As principais origens em função do horário
corridas.taxis.ny.enriq %>% select(posicao_coluna_matriz_origem, posicao_linha_matriz_origem, hora_inicio) %>% group_by(posicao_coluna_matriz_origem, posicao_linha_matriz_origem, hora_inicio) %>% summarise('total' = n()) %>% arrange(desc(total)) -> principais_origens_horario

#Os principais destinos em função do horário
corridas.taxis.ny.enriq %>% select(posicao_coluna_matriz_destino, posicao_linha_matriz_destino, hora_inicio) %>% group_by(posicao_coluna_matriz_destino, posicao_linha_matriz_destino, hora_inicio) %>% summarise('total' = n()) %>% arrange(desc(total)) -> principais_destinos_horario;

#O tempo médio da viagem em função do horário
corridas.taxis.ny.enriq %>% select(hora_inicio, trip_duration) %>% group_by(hora_inicio) %>% summarise('total' = n(),'media_tempo' = mean(trip_duration)) -> horario_duracao_media

#Conforme ilustrado no Shiny

#Analise: pontos_interesse

corridas.taxis.ny.enriq %>% 
  select(posicao_coluna_matriz_origem,posicao_linha_matriz_origem, posicao_coluna_matriz_destino, posicao_linha_matriz_destino, hora_fim, dia_semana_fim, trip_duration) %>% 
  inner_join( pontos_interesse, by = c("posicao_coluna_matriz_destino" = "posicao_coluna_matriz", "posicao_linha_matriz_destino" = "posicao_linha_matriz")) %>% 
  select(Destino = Ponto, DiasSemana = dia_semana_fim, Horario = hora_fim, Duracao = trip_duration, posicao_coluna_matriz = posicao_coluna_matriz_origem,  posicao_linha_matriz = posicao_linha_matriz_origem) -> exp_tabela_destino

corridas.taxis.ny.enriq %>% 
  select(posicao_coluna_matriz_origem,posicao_linha_matriz_origem, posicao_coluna_matriz_destino, posicao_linha_matriz_destino, hora_inicio, dia_semana_inicio, trip_duration) %>% 
  inner_join( pontos_interesse, by = c("posicao_coluna_matriz_origem" = "posicao_coluna_matriz", "posicao_linha_matriz_origem" = "posicao_linha_matriz")) %>% 
  select(Origem = Ponto, DiasSemana = dia_semana_inicio, Horario = hora_inicio, Duracao = trip_duration, posicao_coluna_matriz = posicao_coluna_matriz_destino,  posicao_linha_matriz = posicao_linha_matriz_destino) -> exp_tabela_origem


#tabela_media_tempo por linha destino
exp_tabela_destino %>% inner_join(pontos_interesse, c("posicao_coluna_matriz" = "posicao_coluna_matriz",  "posicao_linha_matriz" = "posicao_linha_matriz")) %>% rename(Origem = Ponto) %>% 
                       select(Destino, Origem, DiasSemana, Horario, Duracao) %>% group_by(Destino, Origem, DiasSemana, Horario) %>% 
                       summarise('total' = n(),'media_tempo' = mean(Duracao)) -> tabela_media_tempo

#tabela_media_tempo por linha origem
exp_tabela_origem %>% inner_join(pontos_interesse, c("posicao_coluna_matriz" = "posicao_coluna_matriz",  "posicao_linha_matriz" = "posicao_linha_matriz")) %>% rename(Destino = Ponto) %>% select(Origem, Destino, DiasSemana, Horario, Duracao) %>% group_by(Origem, Destino, DiasSemana, Horario) %>% summarise('total' = n(),'media_tempo' = mean(Duracao)) -> tabela_media_tempo


#Sugestao
#Utilizar apenas o destino no filtro ou apenas a origem
#tabela_media_tempo por linha destino
#tabela_media_tempo por linha origem
exp_tabela_destino %>% left_join(pontos_interesse, c("posicao_coluna_matriz" = "posicao_coluna_matriz",  "posicao_linha_matriz" = "posicao_linha_matriz")) %>% rename(Origem = Ponto) %>% select(Destino, Origem, DiasSemana, Horario, Duracao) %>% group_by(Destino, Origem, DiasSemana, Horario) %>% summarise('total' = n(),'media_tempo' = mean(Duracao)) -> tabela_media_tempo_destino

exp_tabela_origem %>% left_join(pontos_interesse, c("posicao_coluna_matriz" = "posicao_coluna_matriz",  "posicao_linha_matriz" = "posicao_linha_matriz")) %>% rename(Destino = Ponto) %>% select(Origem, Destino, DiasSemana, Horario, Duracao) %>% group_by(Origem, Destino, DiasSemana, Horario) %>% summarise('total' = n(),'media_tempo' = mean(Duracao)) -> tabela_media_tempo_origem

#Retira linhas que não possuem informações de chuva e neve, bem como corridas com mais de 24h
corridas.taxis.ny.enriq %>% filter(chovendo != '', chovendo != 'indefinido') %>% 
                           filter(nevando != '', nevando != 'Indefinido') %>% filter(trip_duration < 86400) -> corridas.taxis.ny.enriq

#Corrigas e Duracao da viagem por condição do tempo/temperatura
corridas.taxis.ny.enriq %>% select(trip_duration,'temperatura' = temp_media_celsius) %>% 
                            group_by(temperatura) %>% summarise('corridas' = n(),'duracao' = mean(trip_duration)) %>% 
                            arrange(desc(corridas)) %>% mutate('duracao_minutos' = seconds_to_period(duracao)) %>% 
                            select(temperatura, corridas, duracao_minutos) -> corridas_por_temperatura

#Corrigas e Duracao por cond. tempo e horario pico
corridas.taxis.ny.enriq %>% select(hora_inicio, trip_duration,temp_media_celsius) %>% 
                            group_by(hora_inicio) %>% 
                            summarise('corridas' = n(),'temperatura_media' = mean(temp_media_celsius),'tempo_medio' = mean(trip_duration)) %>% 
                            arrange(desc(corridas)) %>% 
                            mutate('tempo_medio_minutos' = seconds_to_period(tempo_medio)) -> corridas_por_tempo_hora



