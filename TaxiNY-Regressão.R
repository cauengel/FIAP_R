# Análise de regressão para estimar a duração das viagens de taxi

library(lubridate)
library(dplyr)
library(gbm)
library(ggplot2)

# Add os indicadores de horário de pico e fim de semana
corridas.taxis.ny.enriq %>% 
  mutate(horario_pico = ifelse(between(hora_inicio, 7, 9) | 
                                 between(hora_inicio, 17, 19), "S", "N")) %>%
  mutate(fim_de_semana = ifelse(dia_semana_inicio == 1 | dia_semana_inicio == 7, "S", "N")) -> corridas.taxis.ny.enriq


# Seleciona as features desejadas
corridas.taxis.ny.enriq %>% 
  select(pickup_longitude, pickup_latitude, dropoff_longitude, dropoff_latitude, dist_geo_km, dist_man_km, dia_mes_inicio, mes_inicio, dia_semana_inicio, hora_inicio, horario_pico, fim_de_semana, trip_duration) -> corridas.taxis.ny.enriq.previsao


# Converte a duração da viagem em escala logarítmica (a dispersão é muito elevada)
corridas.taxis.ny.enriq.previsao <- mutate(corridas.taxis.ny.enriq.previsao, trip_duration = log(trip_duration))


# Converte algumas features em fatores
corridas.taxis.ny.enriq.previsao <- mutate(corridas.taxis.ny.enriq.previsao,
                                           horario_pico = factor(horario_pico),
                                           fim_de_semana = factor(fim_de_semana))


# Split em training set and validation set
set.seed(333)
training <- sample_frac(corridas.taxis.ny.enriq.previsao, 0.8)
validation <- setdiff(corridas.taxis.ny.enriq.previsao, training)

#Regressão Linear Simples

##########################################################
##               Regressão Linear Simples               ##
##########################################################


# Treinamento
regressor_linear.s = lm(formula = trip_duration ~ dist_geo_km,
                        data = training)


# Calc Previsão
linear_pred.s = predict(regressor_linear.s, newdata = validation)


# Plot da previsão p/ uma amostra de n = 5000
plt_linear.s <- tibble(true_duration = exp(validation$trip_duration),
                       pred_duration = exp(linear_pred.s)) %>%
  sample_n(5000) %>%
  ggplot(aes(true_duration, pred_duration)) +
  geom_point()
plt_linear.s + coord_fixed(xlim = c(0,3000), ylim = c(0,3000)) +
  geom_abline(color = "red", slope = 1)


# Verifica se o nível de significância estatística está de acordo (p-value < 0.05)
summary(regressor_linear.s)

#Regressão Linear Múltipla

##########################################################
##              Regressão Linear Múltipla               ##
##########################################################


# Treinamento
regressor_linear.m = lm(formula = trip_duration ~ .,
                        data = training)


# Calc Previsão
linear_pred.m = predict(regressor_linear.m, newdata = validation)


# Plot da previsão p/ uma amostra de n = 5000
plt_linear.m <- tibble(true_duration = exp(validation$trip_duration),
                       pred_duration = exp(linear_pred.m)) %>%
  sample_n(5000) %>%
  ggplot(aes(true_duration, pred_duration)) +
  geom_point()
plt_linear.m + coord_fixed(xlim = c(0,3000), ylim = c(0,3000)) +
  geom_abline(color = "red", slope = 1)

# Verifica se o nível de significância estatística está de acordo (p-value < 0.05)
summary(regressor_linear.m)

#Boosted Decision Tree

##########################################################
##               Boosted Regression Tree                ##
##########################################################


# Treinamento
regressor_boosted <- gbm(trip_duration ~ ., data = training, 
                         bag.fraction = 0.001,
                         distribution = "gaussian", n.trees = 1000,
                         shrinkage = 0.01, interaction.depth = 3)


# Calc Previsão
boosted_pred <- predict(regressor_boosted, newdata = validation, n.trees = 1000)


# Plot da previsão p/ uma amostra de n = 5000
plt_boosted <- tibble(true_duration = exp(validation$trip_duration),
                      pred_duration = exp(boosted_pred)) %>%
  sample_n(5000) %>%
  ggplot(aes(true_duration, pred_duration)) +
  geom_point()
plt_boosted + coord_fixed(xlim = c(0,3000), ylim = c(0,3000)) +
  geom_abline(color = "red", slope = 1)

#Avaliação dos algoritmos de regressão utilizados

##########################################################
##       Avaliação do desempenho dos algoritmos         ##
##########################################################

# Função que retorna o Root Mean Squared Error (RSME)
rmse <- function(erro)
{
  sqrt(mean(erro^2))
}


# Função que retorna o Mean Absolute Error (MAE)
mae <- function(erro)
{
  mean(abs(erro))
}

# Função que retorna o R-quadrado (Rˆ2)
r2 <- function(real, previsto)
{
  1 - (sum((real-previsto)^2) / sum((real-mean(real))^2))
}


# Calcula os erros dos algoritmos de regressão utilizados
error_linear.s <- validation$trip_duration - linear_pred.s
error_linear.m <- validation$trip_duration - linear_pred.m
error_boosted <- validation$trip_duration - boosted_pred


# Calcula o RMSE de cada algoritmo
rmse_linear.s <- rmse(error_linear.s)
rmse_linear.m <- rmse(error_linear.m)
rmse_boosted <- rmse(error_boosted)


# Calcula o MAE de cada algoritmo
mae_linear.s <- mae(error_linear.s)
mae_linear.m <- mae(error_linear.m)
mae_boosted <- mae(error_boosted)


# Calcula o R-quadrado de cada algoritmo
r2_linear.s <- r2(validation$trip_duration, linear_pred.s)
r2_linear.m <- r2(validation$trip_duration, linear_pred.m)
r2_boosted <- r2(validation$trip_duration, boosted_pred)


# Nos três indicadores utilizados o boosted regression tree apresenta melhores resultados
# dos que os demais (menor RMSE e MAE e um maior coeficiente de determinação (Rˆ2))


#Aplicação do algoritmo boosted regression tree no test set

##########################################################
##              Carregar Dados para Teste               ##
##########################################################

#Carrega o conjunto de teste das corridas de taxi
arquivo_corridas_taxi_teste = "test.csv"
setwd(diretorio_arq_dados)
corridas.taxis.ny.teste <- read.table(arquivo_corridas_taxi_teste,sep=",",header=TRUE)

#Exclui os outliers e aquelas viagens que estejam fora dos limites de Nova York
corridas.taxis.ny.teste.sem.out <- filtra_corridas(corridas.taxis.ny.teste)

# Add distância euclidiana, distância de manhattan e splits de data e tempo
corridas.taxis.ny.teste.sem.out <- corridas.taxis.ny.teste.sem.out %>%
  mutate(dist_geo_km = dist_geometrica_km(pickup_longitude, pickup_latitude, dropoff_longitude, dropoff_latitude),
         dist_man_km = dist_manhattan_km(pickup_longitude, pickup_latitude, dropoff_longitude, dropoff_latitude),
         dia_mes_inicio = obtem_dia_data(pickup_datetime),
         mes_inicio = obtem_mes_data(pickup_datetime),
         dia_semana_inicio = obtem_dia_semana_data(pickup_datetime),
         hora_inicio = obtem_hora_data(pickup_datetime))


# Add os indicadores de horário de pico e fim de semana
corridas.taxis.ny.teste.sem.out %>%
  mutate(horario_pico = ifelse(between(hora_inicio, 7, 9) |
                                 between(hora_inicio, 17, 19), "S", "N")) %>%
  mutate(fim_de_semana = ifelse(dia_semana_inicio == 1 | dia_semana_inicio == 7, "S", "N")) -> corridas.taxis.ny.teste.sem.out


# Seleciona as features desejadas
corridas.taxis.ny.teste.sem.out %>%
  select(pickup_longitude, pickup_latitude, dropoff_longitude, dropoff_latitude, dist_geo_km, dist_man_km, dia_mes_inicio, mes_inicio, dia_semana_inicio, hora_inicio, horario_pico, fim_de_semana) -> corridas.taxis.ny.teste.sem.out


# Converte algumas features em fatores
corridas.taxis.ny.teste.sem.out <- mutate(corridas.taxis.ny.teste.sem.out,
                                          horario_pico = factor(horario_pico),
                                          fim_de_semana = factor(fim_de_semana))


##########################################################
##               Boosted Regression Tree                ##
##########################################################

# Calc Previsão
boosted_pred.test <- predict(regressor_boosted, newdata = corridas.taxis.ny.teste.sem.out, n.trees = 1000)

# Add duração da viagem prevista para o dataset de teste
corridas.taxis.ny.teste.sem.out %>% 
  mutate(trip_duration_predicted = round(exp(boosted_pred.test), digits = 0)) -> corridas.taxis.ny.teste.previsao

