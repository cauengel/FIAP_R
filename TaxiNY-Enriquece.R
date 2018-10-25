
library(dplyr)
library(lubridate)





# Converte graus para radianos
deg2rad <- function(deg) return(deg*pi/180)


#Distância geométrica utilizando a fórmula de Haversine em quilômetros
dist_geometrica_km <- function(long_ori,lat_ori,long_dest,lat_dest){
  R <- 6371 # Raio médio da Terra em Km
  long1 <- deg2rad(long_ori)
  long2 <- deg2rad(long_dest)
  lat1  <- deg2rad(lat_ori)
  lat2  <- deg2rad(lat_dest)
  
  
  dlon <- long2 - long1
  dlat<- lat2 - lat1
  a <- sin(dlat/2)^2 + cos(lat1) * cos(lat2) * sin(dlon/2)^2
  c <- 2 * atan2( sqrt(a), sqrt(1-a) ) 
  d <- R * c 
  
  
  return(d) # Distância em km
}

# Calcula distância geométrica utilizando a fórmula de Haversine em metros
dist_geometrica_mt <-function(long_ori,lat_ori,long_dest,lat_dest){
  return (dist_geometrica_km(long_ori,lat_ori,long_dest,lat_dest)*1000)
}



#Gera as informações sobre NY necessárias para transformar a cidade em uma matriz
#mapeia todas as latitudes e longitudes que irão compor a matriz
gera_lista_ny <- function(tabelaTrips,tamQuadradoNY) {
  
  #Obtem as menores e maiores latitudes e longitudes
  menor_latitude = min(c(min(tabelaTrips$pickup_latitude),min(tabelaTrips$dropoff_latitude)))
  
  maior_latitude = max(c(max(tabelaTrips$pickup_latitude),max(tabelaTrips$dropoff_latitude)))
  
  
  menor_longitude = min(c(min(tabelaTrips$pickup_longitude),min(tabelaTrips$dropoff_longitude)))
  
  
  maior_longitude = max(c(max(tabelaTrips$pickup_longitude),max(tabelaTrips$dropoff_longitude)))
  
  
  
  #Calcula o tamanho de ny em metros 
  distancia_metros_longitude_ny = dist_geometrica_mt(menor_longitude,maior_latitude,maior_longitude,maior_latitude)
  
  
  distancia_metros_latitude_ny=dist_geometrica_mt(maior_longitude,menor_latitude,maior_longitude,maior_latitude)
  
  
  
  #Calcula o tamanho de ny em graus
  graus_longitude_ny = maior_longitude - menor_longitude
  
  graus_latitude_ny = maior_latitude - menor_latitude
  
  
  #Calcula quantos graus corresponde a um metro 
  graus_longitude_1_metro = graus_longitude_ny / distancia_metros_longitude_ny
  
  graus_latitude_1_metro = graus_latitude_ny / distancia_metros_latitude_ny
  
  
  #Calcula a quantidade de graus para fazer o quadrado da matriz
  graus_longitude_quadrado = tamQuadradoNY * graus_longitude_1_metro
  graus_latitude_quadrado = tamQuadradoNY * graus_latitude_1_metro
  
  #Calcula quantas colunas e linhas a matriz terá 
  quant_colunas = as.integer(graus_longitude_ny / graus_longitude_quadrado) + 1
  quant_linhas = as.integer(graus_latitude_ny / graus_latitude_quadrado) + 1
  
  
  #Calcula todas as longitudes 
  longitudes <- 1:quant_colunas
  longitudes <- longitudes * graus_longitude_quadrado
  longitudes <- longitudes + menor_longitude
  
  
  #Calcula todas as latitudes 
  latitudes <- 1:quant_linhas
  latitudes <- latitudes * graus_latitude_quadrado
  latitudes <- latitudes + menor_latitude
  
  
  #Calcula a equação de PA da longitude
  pa_longitudes <-c(menor_longitude+graus_longitude_quadrado,graus_longitude_quadrado,quant_colunas)
  names(pa_longitudes) <- c("a1","r","max_elemen")
  
  #Calcula a equação de PA da latitude
  pa_latitudes <-c(menor_latitude+graus_latitude_quadrado,graus_latitude_quadrado,quant_linhas)
  names(pa_latitudes) <- c("a1","r","max_elemen")
  
  #Devolve lista com todas as longitudes, todas as latitudes, equação pa de longitutde, equação pa de latitudes  
  m <- list(longitudes,latitudes,pa_longitudes,pa_latitudes)
  
  return (m)
  
}



#Retorna equação da reta passando entre dois pontos p1 e p2
#equação y = a.x + b
obtem_equacao_reta <- function(x1,y1,x2,y2){
  a_vet <- (y2 -y1) / (x2 - x1)
  b_vet <- ((-1) * a_vet * x1) + y1
  return (data.frame(a=a_vet,b=b_vet))
}


#Calcula o caminho na matriz_deslocamento referente a uma viagem de taxi
#as informações relevantes foram passadas na lista que esta função recebe 
calcula_caminho_1_reta<-function(reta){
  
  #Recupera as informações necessárias
  a = reta[[1]]
  b = reta[[2]]
  linha_inicial = reta[[3]]
  coluna_inicial = reta[[4]]
  linha_final = reta[[5]]
  coluna_final = reta[[6]]
  n_viagem = reta[[7]]
  
  #Linhas e colunas que serão analisadas para verificar se há intersecção
  linhas = linha_inicial:linha_final
  colunas = coluna_inicial:coluna_final
  
  
  #Se é uma reta paralela a longitude, basta incluir todos os elementos na matriz_deslocamento  
  if (is.infinite(a) || is.nan(a)){
    #Precisa tratar o caso quando a reta é paralela as longitudes, longitude inicial = longitude final
    #Nesse caso a rota são todas as linhas e colunas da matriz, portanto basta marcar na matriz de deslocamento
    matriz_deslocamento[linhas,colunas] <<- matriz_deslocamento[linhas,colunas]+1
    
  }
  else { 
    #Caso seja uma reta que cruze o eixo da longitude 
    
    #Recupera os limites inferiores e superiores das longitudes e latitudes 
    limite_superior_x = longitudes[colunas]
    limite_superior_y = latitudes[linhas]
    limite_inferior_x = -180
    limite_inferior_y = -90
    
    #Caso ocorra na coluna 1 realiza os ajustes 
    if ((coluna_inicial <= 1) || (coluna_final <= 1)){
      #Caso seja só um elemento, o limite inferior de x é -180 que é a menor longitude possível 
      #Caso seja mais de um elemento inclui -180 no primeiro elemento e obtem os anteriores 
      if(length(colunas)>1){
        if (coluna_inicial <= 1){
          colunas_aux = head(colunas,length(colunas)-1)
          limite_inferior_x <- c(-180,longitudes[colunas_aux])
        }
        else { #coluna_final <= 1
          colunas_aux = tail(colunas,length(colunas)-1)
          limite_inferior_x <- c(longitudes[colunas_aux],-180)
        }
      }
      
    }
    else {
      #Caso não seja a coluna 1, calcula os limites inferiores da forma tradicional 
      colunas_aux <- (colunas - 1)
      limite_inferior_x = longitudes[colunas_aux]
    }
    
    #Mesmo tratamento da coluna para a linha 1
    if ((linha_inicial <= 1) || (linha_final <= 1)){
      #Caso seja só um elemento, o limite inferior de y é -90 que é a menor latitude possível 
      #Caso seja mais de um elemento inclui -90 no primeiro elemento e obtem os anteriores 
      if(length(linhas)>1){
        if (linha_inicial <= 1){
          linhas_aux = head(linhas,length(linhas)-1)
          limite_inferior_y = c(-90,latitudes[linhas_aux])
        }
        else { #linha_final <= 1
          linhas_aux = tail(linhas,length(linhas)-1)
          limite_inferior_y = c(latitudes[linhas_aux],-90)
        }
      }
      
    }
    else {
      #Caso não seja a linha 1 realiza o cálculo tradicional para o limite inferior
      limite_inferior_y = latitudes[linhas-1]
    }
    
    
    #Calcula os elementos de x que pertencem a reta superior de y 
    x_equacao_sup = (limite_superior_y - b) / a
    
    
    #Transforma em vetor a comparação para verificar se os elementos de x pertencem a alguma posição na matriz deslocamento
    vet_aux_x_equacao <- rep(x_equacao_sup,length(limite_superior_x))
    vet_aux_limite_sup_x <- rep(limite_superior_x,each=length(x_equacao_sup))
    vet_aux_limite_inf_x <- rep(limite_inferior_x,each=length(x_equacao_sup))
    
    #Verifica se estão dentro dos limites de longitude
    vet_log_sup_x <- vet_aux_x_equacao < vet_aux_limite_sup_x
    vet_log_inf_x <- vet_aux_x_equacao > vet_aux_limite_inf_x
    
    #Realiza um AND lógico para garantir que sejam menores que o limite superior e maiores que o inferior
    vet_log_result <- vet_log_sup_x & vet_log_inf_x
    
    
    
    #Calcula os elementos de x que pertencem a reta inferior de y
    x_equacao_inf = (limite_inferior_y - b) / a
    
    
    #Transforma em vetor a comparação para verificar se os elementos de x pertencem a alguma posição na matriz deslocamento
    vet_aux_x_equacao <- rep(x_equacao_inf,length(limite_superior_x))
    
    #Verifica se estão dentro dos limites de longitude
    vet_log_sup_x <- vet_aux_x_equacao < vet_aux_limite_sup_x
    vet_log_inf_x <- vet_aux_x_equacao > vet_aux_limite_inf_x
    
    #Realiza um AND lógico para garantir que sejam menores que o limite superior e maiores que o inferior
    vet_log_result2 <- vet_log_sup_x & vet_log_inf_x
    
    #Realiza um OR lógico para contabilizar se está presente na equação de cima ou de baixo 
    vet_log_result <- (vet_log_result2 | vet_log_result) 
    
    #Transforma em matriz
    matriz_aux_x_equacao <- matrix(vet_log_result,nrow=length(linhas))
    #Soma 1 quando TRUE e 0 quando FALSE 
    matriz_deslocamento[linhas,colunas] <<- matriz_deslocamento[linhas,colunas] + matriz_aux_x_equacao
    
    
    
    
    #Faz raciocínio análogo para os y que cruzam a reta x superior
    y_equacao_sup = (a * limite_superior_x) + b
    
    #Transforma em vetor
    vet_aux_y_equacao <- rep(y_equacao_sup,length(limite_superior_y))
    vet_aux_limite_sup_y <- rep(limite_superior_y,each=length(y_equacao_sup))
    vet_aux_limite_inf_y <- rep(limite_inferior_y,each=length(y_equacao_sup))
    
    #Verifica se está dentro dos limites
    vet_log_sup_y <- vet_aux_y_equacao < vet_aux_limite_sup_y
    vet_log_inf_y <- vet_aux_y_equacao > vet_aux_limite_inf_y
    
    vet_log_result <- vet_log_sup_y & vet_log_inf_y
    
    
    
    
    #Faz raciocínio análogo para os y que cruzam a reta x inferior
    y_equacao_inf = (a * limite_inferior_x) + b
    
    
    #Transforma em vetor
    vet_aux_y_equacao <- rep(y_equacao_inf,length(limite_inferior_y))
    
    #Verifica se está dentro dos limites
    vet_log_sup_y <- vet_aux_y_equacao < vet_aux_limite_sup_y
    vet_log_inf_y <- vet_aux_y_equacao > vet_aux_limite_inf_y
    
    #Verifica se o y está dentro dos limites inferiores e superiores esperados para as posiçoes da matriz   
    vet_log_result2 <- vet_log_sup_y & vet_log_inf_y
    
    #Faz um OR lógico para obter tanto os que estavam cruzando a reta x superior bem como a reta x inferior
    vet_log_result <- (vet_log_result2 | vet_log_result)
    
    #Transforma em matriz
    matriz_aux_y_equacao <- matrix(vet_log_result,nrow=length(linhas),byrow = TRUE)
    
    #impede de contabilizar duas vezes o mesmo elemento
    matriz_aux_y_equacao <- matriz_aux_y_equacao & (!matriz_aux_x_equacao)
    
    #Soma 1 quando TRUE e 0 quando FALSE 
    matriz_deslocamento[linhas,colunas] <<- matriz_deslocamento[linhas,colunas] + matriz_aux_y_equacao
    
    
    
  } #Else para tratamento de retas que cruzam o eixo das longitudes
  
  return(NULL)
}

#Transforma informações de um data frame para uma lista a ser processada para o cálculo de deslocamentos 
gera_lista_para_retas<-function(linha){
  a <- linha[[1]]
  b <- linha[[2]]
  linha_origem <- linha[[3]]
  coluna_origem <- linha[[4]]
  linha_destino <- linha[[5]]
  coluna_destino <- linha[[6]]
  n_viagem <- linha[[7]]
  return(list(a,b,linha_origem,coluna_origem,linha_destino,coluna_destino,n_viagem))
}

#Calcula os deslocamentos dos taxis na matriz de deslocamento
#P.S: aqui se utiliza a variável global matriz_deslocamento porque a função utilizada no lapply não permite que essa
#seja passada como parâmetro 
calc_matriz_deslocamento <- function(df,lista_ny){
  
  pa_longitudes = lista_ny[[3]]
  pa_latitudes = lista_ny[[4]]
  
  n_linhas   = pa_latitudes["max_elemen"]
  n_colunas  = pa_longitudes["max_elemen"]
  
  
  #Variável global criada com valor zero em todas as células e na dimensão correta 
  matriz_deslocamento <<- matrix(0,nrow=n_linhas,ncol=n_colunas)
  
  #Obtem as equações das retas de cada viagem
  df_reta <- obtem_equacao_reta(df$pickup_longitude,df$pickup_latitude,df$dropoff_longitude,df$dropoff_latitude)
  
  
  n_viagens = nrow(df_reta)
  df_reta$linha_origem = df[,"posicao_linha_matriz_origem"]
  df_reta$coluna_origem = df[,"posicao_coluna_matriz_origem"]
  df_reta$linha_destino = df[,"posicao_linha_matriz_destino"]
  df_reta$coluna_destino = df[,"posicao_coluna_matriz_destino"]
  df_reta$n_viagem = 1:n_viagens
  
  
  #Escolhe uma amostra de 100.000 elementos
  n_tam_amostra = 100000
  #Sorteia uma amostra aleatório com 100.000 elementos, cada elemento contendo um índice de
  #viagem que será usado na amostra
  vet_amostra = sample(1:n_viagens,n_tam_amostra,replace = FALSE)
  
  df_amostra = df_reta[vet_amostra,]
  
  #Transforma as informações para o formato de lista 
  lista_viagens <- apply(df_amostra,1,FUN=gera_lista_para_retas)
  
  #Aplica o processamento para cada viagem para cada elemento da lista 
  print("Hora de aplicar a cada caminho da amostra utilizando as retas ....")
  Sys.time()
  lapply(lista_viagens,FUN=calcula_caminho_1_reta)
  
  return(NULL)
  
  
}


#Distância de manhattan utilizando a soma dos catetos, considerando a distância geométrica como sendo a hipotenusa 
dist_manhattan_km <- function(long_ori,lat_ori,long_dest,lat_dest){
  return(dist_geometrica_km(long_ori,lat_ori,long_ori,lat_dest) + 
           dist_geometrica_km(long_ori,lat_dest,long_dest,lat_dest))
}

#Calcula as posições das colunas na matriz de deslocamento de um vetor com as longitudes
calc_pos_coluna <- function(longitude,lista_ny){
  pa_longitudes = lista_ny[[3]]
  a1 <- pa_longitudes["a1"]
  r <- pa_longitudes["r"]
  coluna_real <- ((longitude-a1+r)/r)
  coluna_int <- as.integer(coluna_real)
  coluna_int[coluna_int != coluna_real] <- coluna_int[coluna_int != coluna_real] + 1
  return (coluna_int)
  
}

#Calcula as posições das linhas na matriz de deslocamento de um vetor com as latitudes
calc_pos_linha <- function(latitude,lista_ny){
  pa_latitudes = lista_ny[[4]]
  a1 <- pa_latitudes["a1"]
  r <- pa_latitudes["r"]
  linha_real <- ((latitude-a1+r)/r)
  linha_int <- as.integer(linha_real)
  linha_int[linha_int != linha_real] <- linha_int[linha_int != linha_real] + 1
  return (linha_int)
  
}

#Obtem o dia do mês de uma data simplificada (dia mês ano)
obtem_dia_data_simplif <-function(data_informada){
  data_convertida = dmy(data_informada)
  return(mday(data_convertida))
}

#Obtem o dia do mês de uma data 
obtem_dia_data <-function(data_informada){
  data_convertida = ymd_hms(data_informada)
  return(mday(data_convertida))
}

#Obtem o mês de uma data no formato simplificado (dia mês e ano)
obtem_mes_data_simplif <-function(data_informada){
  data_convertida = dmy(data_informada)
  return(month(data_convertida))
}

#Obtem o mês de uma data 
obtem_mes_data <-function(data_informada){
  data_convertida = ymd_hms(data_informada)
  return(month(data_convertida))
}

#Obtem o dia da semana de uma data 
obtem_dia_semana_data <-function(data_informada){
  data_convertida = ymd_hms(data_informada)
  return(wday(data_convertida))
}

#Obtem a hora de uma data
obtem_hora_data <-function(data_informada){
  data_convertida = ymd_hms(data_informada)
  return(hour(data_convertida))
}



#Enriquece as informações sobre o clima, convertendo os graus para celsius
#e incluindo uma informação indicando se estava chovendo ou nevando no dia
filtra_e_enriquece_clima<- function(){
  
  ########################################################################################################################
  ###### CLIMA OBTIDO A PARTIR DO KAGGLE NA URL: https://www.kaggle.com/mathijs/weather-data-in-new-york-city-2016   #####
  ########################################################################################################################
  
  
  #Le o arquivo com os dados do clima
  arquivo_clima_ny = "weather_data_nyc_2016.csv"
  clima.ny <- read.table(arquivo_clima_ny,sep=",",header=TRUE)
  
  #Inclui informações sobre dia do mês, mês e temperatura média em celsius
  clima.ny %>%  mutate(dia_mes=obtem_dia_data_simplif(date)) -> clima.ny.enriq
  clima.ny.enriq %>%  mutate(mes=obtem_mes_data_simplif(date)) -> clima.ny.enriq
  clima.ny.enriq %>%  mutate(temp_media_celsius=(average.temperature-32) / 1.8) -> clima.ny.enriq
  
  #Inclui NA para todos os dados que continham o valor "T"
  clima.ny.enriq[which(clima.ny.enriq$precipitation == "T"),"precipitation"] <- NA
  
  #Converte a precipitação de chuva em informação numérica
  clima.ny.enriq$prep_chuva <- as.numeric(as.character(clima.ny.enriq$precipitation))
  
  #Repete os mesmos passos para a precipitação de neve
  clima.ny.enriq[which(clima.ny.enriq$snow.fall == "T"),"snow.fall"] <- NA
  
  clima.ny.enriq$prep_neve <- as.numeric(as.character(clima.ny.enriq$snow.fall))
  
  
  #Inclui a dedução da presença de neve e chuva durante o dia  
  clima.ny.enriq$nevando <- "Nao"
  clima.ny.enriq[which(clima.ny.enriq$prep_neve > 0),"nevando"] <- "Sim"
  #E para quando há NA indica como indefinida a situação sobre a neve
  clima.ny.enriq[which(is.na(clima.ny.enriq$prep_neve)),"nevando"] <- "Indefinido"
  
  
  #Repete os mesmos passos para a chuva
  clima.ny.enriq$chovendo <- "Nao"
  clima.ny.enriq[which(clima.ny.enriq$prep_chuva > 0),"chovendo"] <- "Sim"
  clima.ny.enriq[which(is.na(clima.ny.enriq$prep_chuva)),"chovendo"] <- "Indefinido"
  
  
  return(clima.ny.enriq)
  
}


#Acrescenta as informações sobre o clima no data.frame sobre as corridas
gera_info_clima <- function(df_clima,df_corridas){
  df <- df_corridas
  
  df_clima %>% select(dia_mes,mes,prep_chuva,chovendo,prep_neve,nevando,temp_media_celsius) -> df_clima_aux 
  
  df <- left_join(df,df_clima_aux,by=c("mes_inicio"="mes",
                                       "dia_mes_inicio"="dia_mes"))
  
  
  return(df)
}


#Enriquece os dados das corridas obtidas e filtradas com as informações sobre as festas ocorridas 
#em ny em 2016 e as informações sobre o clima em 2016 
enriquece_dados <- function(df_raw,lista_ny) {
  
  df <- df_raw 
  
  #Inclui informações sobre as distâncias geométricas e de manhattan nas corridas de taxi
  #Além das informações sobre a localização na matriz_deslocamento dessas corridas 
  #Faz a compilação das datas de origem e término da viagem em variáveis que capturam o dia do mês, 
  #dia da semana, hora, etc. 
  df %>% mutate(dist_geo_km = dist_geometrica_km(pickup_longitude,pickup_latitude,dropoff_longitude,dropoff_latitude)) -> df   
  df %>% mutate(dist_man_km = dist_manhattan_km(pickup_longitude,pickup_latitude,dropoff_longitude,dropoff_latitude)) -> df
  df %>% mutate(posicao_coluna_matriz_origem=calc_pos_coluna(pickup_longitude,lista_ny)) -> df
  df %>% mutate(posicao_linha_matriz_origem=calc_pos_linha(pickup_latitude,lista_ny)) -> df
  df %>% mutate(posicao_coluna_matriz_destino=calc_pos_coluna(dropoff_longitude,lista_ny)) -> df
  df %>% mutate(posicao_linha_matriz_destino=calc_pos_linha(dropoff_latitude,lista_ny)) -> df
  df %>% mutate(dia_mes_inicio=obtem_dia_data(pickup_datetime)) -> df
  df %>% mutate(mes_inicio=obtem_mes_data(pickup_datetime)) -> df
  df %>% mutate(dia_semana_inicio=obtem_dia_semana_data(pickup_datetime)) -> df  
  df %>% mutate(hora_inicio=obtem_hora_data(pickup_datetime)) -> df
  df %>% mutate(dia_mes_fim=obtem_dia_data(dropoff_datetime)) -> df
  df %>% mutate(mes_fim=obtem_mes_data(dropoff_datetime)) -> df
  df %>% mutate(dia_semana_fim=obtem_dia_semana_data(dropoff_datetime)) -> df  
  df %>% mutate(hora_fim=obtem_hora_data(dropoff_datetime)) -> df
  
  
  #Enriquece com os dados do clima de ny ocorrido em 2016
  df_clima.enriq <- filtra_e_enriquece_clima()
  df <- gera_info_clima(df_clima.enriq,df)
  
  return (df)
  
  
}

#Filtra as corridas lidas do arquivo
#Exclui os outliers conforme a distribuição obtida pela função boxplot
#e exclui todas as viagens que não ocorram dentro das coordenadas de NY
filtra_corridas <- function(df){
  
  # Coordenadas da cidade de ny https://www.latlong.net/place/new-york-city-ny-usa-1848.html
  # Longitude -73 Latitude 40
  longitude_ny = -73
  latitude_ny = 40
  longitude_ny_min = -74.15
  longitude_ny_max = -73
  latitude_ny_min = 40 
  latitude_ny_max = 41
  duracao_min_corrida = 60    
  
  df_aux <-df
  
  #Obtem as estatísticas sobre as distribuições das latitudes e longitudes das corridas para excluir aquelas que são outliers
  estat_pick_long <- boxplot.stats(df_aux$pickup_longitude)
  estat_pick_lat <- boxplot.stats(df_aux$pickup_latitude)
  estat_drop_long <- boxplot.stats(df_aux$dropoff_longitude)
  estat_drop_lat <- boxplot.stats(df_aux$dropoff_latitude)
  
  #Inclui na lista das corridas que serão excluídas todas aquelas que estão na lista dos outliers segundo o boxplot
  out_liers <- union( which(df_aux$pickup_longitude %in% estat_pick_long$out),
                      which(df_aux$pickup_latitude %in% estat_pick_lat$out)) 
  
  out_liers <- union(out_liers,
                     which(df_aux$dropoff_longitude %in% estat_drop_long$out) ) 
  
  out_liers <- union(out_liers,
                     which(df_aux$dropoff_latitude %in% estat_drop_lat$out) )
  
  #Excluir viagens que não são em nova york
  
  #Exclui longitude fora de ny
  out_liers <- union(out_liers,
                     which(df_aux$pickup_longitude < longitude_ny_min | df_aux$pickup_longitude > longitude_ny_max ) )
  
  
  
  out_liers <- union(out_liers,
                     which(df_aux$dropoff_longitude < longitude_ny_min | df_aux$dropoff_longitude > longitude_ny_max ) )
  
  #Exclui latitudes fora de ny
  out_liers <- union(out_liers,
                     which(df_aux$pickup_latitude < latitude_ny_min | df_aux$pickup_latitude > latitude_ny_max ) )
  
  out_liers <- union(out_liers,
                     which(df_aux$dropoff_latitude < latitude_ny_min | df_aux$dropoff_latitude > latitude_ny_max ) )
  
  
  #Retira as corridas com duração menor que a mínima esperada. No caso, escolhemos retirar aquelas com duração inferior a 
  #um minutos (60 segundos)
  out_liers <- union(out_liers, 
                     which(df_aux$trip_duration < duracao_min_corrida))
  
  df_aux <- df_aux[(-1)* out_liers,] 
  
  return(df_aux)
  
}

filtra_corridas_manhattan <- function(df){
  
  # Coordenadas da cidade de ny https://www.latlong.net/place/new-york-city-ny-usa-1848.html
  # Longitude -73 Latitude 40
  longitude_manhattan_min = -74.0031
  longitude_manhattan_max = -73.9747
  latitude_manhattan_min = 40.7019 
  latitude_manhattan_max = 40.8771
  
  
  df_aux <-df
  
  
  
  
  #Excluir viagens que não são em manhattan
  
  #Exclui longitude fora de manhattan
  out_liers <- union(which(df_aux$dropoff_longitude < longitude_manhattan_min | 
                             df_aux$dropoff_longitude > longitude_manhattan_max ),
                     which(df_aux$pickup_longitude < longitude_manhattan_min | 
                             df_aux$pickup_longitude > longitude_manhattan_max ) )
  
  
  
  out_liers <- union(out_liers,
                     which(df_aux$dropoff_longitude < longitude_manhattan_min | 
                             df_aux$dropoff_longitude > longitude_manhattan_max ) )
  
  #Exclui latitudes fora de manhattan
  out_liers <- union(out_liers,
                     which(df_aux$pickup_latitude < latitude_manhattan_min | 
                             df_aux$pickup_latitude > latitude_manhattan_max ) )
  
  out_liers <- union(out_liers,
                     which(df_aux$dropoff_latitude < latitude_manhattan_min | 
                             df_aux$dropoff_latitude > latitude_manhattan_max ) )
  
  
  
  df_aux <- df_aux[(-1)* out_liers,] 
  
  return(df_aux)
  
}


#WARNING SOBRE O TEMPO DE EXECUÇÃO 
print("##  Devido ao cálculo de rotas de taxi, o tempo de execução deverá ser em torno de 15 minutos ##")

#Imprime início da execução
print("Data/hora de início de execução")
print(Sys.time())


#Atribui o diretorio corrente como sendo o diretorio de dados para realizar a leitura das informações 
setwd(diretorio_arq_dados)

#Carrega os dados sobre corridas de taxi
arquivo_corridas_taxi = "train.csv"
corridas.taxis.ny.raw <- read.table(arquivo_corridas_taxi,sep=",",header=TRUE)

#Exclui os outliers e aquelas viagens que estejam fora dos limites de Nova York
corridas.taxis.ny.sem.out <- filtra_corridas(corridas.taxis.ny.raw)

#Gera informações para transformar Nova York em uma representação matricial 
lista_ny <- gera_lista_ny(corridas.taxis.ny.sem.out,14)

#Longitudes e latitudes também serão variáveis globais
longitudes = lista_ny[[1]]
latitudes = lista_ny[[2]]



#Enriquece os dados das corridas com informações próprias e aquelas obtidas em outros arquivos
corridas.taxis.ny.enriq <- enriquece_dados(corridas.taxis.ny.sem.out,lista_ny)


#Cria variável global que irá conter os deslocamentos de taxi pela cidade de ny contabilizados 
#A cada vez que passam por uma posição da matriz será contabilizado mais 1 para aquela posição
#Isto é repetido para cada corrida feita
matriz_deslocamento <-matrix()

#Gera matriz com o numero de vezes que um taxi passa por um determinado ponto da matriz
calc_matriz_deslocamento(corridas.taxis.ny.enriq,lista_ny)


#Imprime final da execução
print("Data/hora do término de execução")
print(Sys.time())





