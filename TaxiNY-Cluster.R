
#Clusterização

##########################################################
##  Clusterização dos locais de embarque e desembarque  ##
##########################################################

# Cria clusters dos locais de embarque e desembarque dentro e fora do horário de pico

library(dplyr)
library(leaflet)

# Seleciona as features desejadas
corridas.taxis.ny.enriq %>% 
  select(id, pickup_longitude, pickup_latitude, dropoff_longitude, dropoff_latitude, horario_pico) -> corridas.taxis.ny.enriq.cluster


# Converte horário de pico em fator
corridas.taxis.ny.enriq.cluster <- mutate(corridas.taxis.ny.enriq.cluster,
                                          horario_pico = factor(horario_pico))


# Split do dataset em horário de pico e fora do pico, além da divisão entre embarque e desembarque
corridas.taxis.ny.enriq.cluster.pico.embarque <- corridas.taxis.ny.enriq.cluster %>%
  filter(horario_pico == "S") %>% 
  select(pickup_longitude, pickup_latitude, id)

corridas.taxis.ny.enriq.cluster.pico.desembarque <- corridas.taxis.ny.enriq.cluster %>%
  filter(horario_pico == "S") %>% 
  select(dropoff_longitude, dropoff_latitude, id)

corridas.taxis.ny.enriq.cluster.forapico.embarque <- corridas.taxis.ny.enriq.cluster %>%
  filter(horario_pico == "N") %>% 
  select(pickup_longitude, pickup_latitude, id)

corridas.taxis.ny.enriq.cluster.forapico.desembarque <- corridas.taxis.ny.enriq.cluster %>%
  filter(horario_pico == "N") %>% 
  select(dropoff_longitude, dropoff_latitude, id)

# Clusteriza o subset com os dados do horário de pico referente ao embarque

kms_peak_pickup <- kmeans(cbind(corridas.taxis.ny.enriq.cluster.pico.embarque$pickup_longitude, corridas.taxis.ny.enriq.cluster.pico.embarque$pickup_latitude), centers=10)
cluster_peak_pickup <- cbind(corridas.taxis.ny.enriq.cluster.pico.embarque, kms_peak_pickup$cluster)
names(cluster_peak_pickup) <- c("lng", "lat", "id", "cluster_id")
c_peak_pickup <- cluster_peak_pickup %>%
  group_by(cluster_id) %>%
  summarise_at(vars(id), n_distinct) %>%
  mutate(lng=kms_peak_pickup$centers[,1], lat=kms_peak_pickup$centers[,2])

kmeansMap_peak_pickup <- leaflet(c_peak_pickup) %>% 
  addTiles() %>% 
  addCircleMarkers(lng=~lng, lat=~lat,
                   popup=as.character(c_peak_pickup$id),
                   radius=(c_peak_pickup$id/1000),
                   clusterOptions = markerClusterOptions())

kmeansMap_peak_pickup


# Clusteriza o subset com os dados do horário de pico referente ao desembarque

kms_peak_dropoff <- kmeans(cbind(corridas.taxis.ny.enriq.cluster.pico.desembarque$dropoff_longitude, corridas.taxis.ny.enriq.cluster.pico.desembarque$dropoff_latitude), centers=10)
cluster_peak_dropoff <- cbind(corridas.taxis.ny.enriq.cluster.pico.desembarque, kms_peak_dropoff$cluster)
names(cluster_peak_dropoff) <- c("lng", "lat", "id", "cluster_id")
c_peak_dropoff <- cluster_peak_dropoff %>%
  group_by(cluster_id) %>%
  summarise_at(vars(id), n_distinct) %>%
  mutate(lng=kms_peak_dropoff$centers[,1], lat=kms_peak_dropoff$centers[,2])

kmeansMap_peak_dropoff <- leaflet(c_peak_dropoff) %>% 
  addTiles() %>% 
  addCircleMarkers(lng=~lng, lat=~lat,
                   popup=as.character(c_peak_dropoff$id),
                   radius=(c_peak_dropoff$id/1000),
                   clusterOptions = markerClusterOptions())

kmeansMap_peak_dropoff


# Clusteriza o subset com os dados do horário fora de pico referente ao embarque

kms_offpeak_pickup <- kmeans(cbind(corridas.taxis.ny.enriq.cluster.forapico.embarque$pickup_longitude, corridas.taxis.ny.enriq.cluster.forapico.embarque$pickup_latitude), centers=10)
cluster_offpeak_pickup <- cbind(corridas.taxis.ny.enriq.cluster.forapico.embarque, kms_offpeak_pickup$cluster)
names(cluster_offpeak_pickup) <- c("lng", "lat", "id", "cluster_id")
c_offpeak_pickup <- cluster_offpeak_pickup %>%
  group_by(cluster_id) %>%
  summarise_at(vars(id), n_distinct) %>%
  mutate(lng=kms_offpeak_pickup$centers[,1], lat=kms_offpeak_pickup$centers[,2])

kmeansMap_offpeak_pickup <- leaflet(c_offpeak_pickup) %>% 
  addTiles() %>% 
  addMarkers(lng=~lng, lat=~lat,
             popup=as.character(c_offpeak_pickup$id),
             clusterOptions = markerClusterOptions())

kmeansMap_offpeak_pickup


# Clusteriza o subset com os dados do horário fora de pico referente ao desembarque

kms_offpeak_dropoff <- kmeans(cbind(corridas.taxis.ny.enriq.cluster.forapico.desembarque$dropoff_longitude, corridas.taxis.ny.enriq.cluster.forapico.desembarque$dropoff_latitude), centers=10)
cluster_offpeak_dropoff <- cbind(corridas.taxis.ny.enriq.cluster.forapico.desembarque, kms_offpeak_dropoff$cluster)
names(cluster_offpeak_dropoff) <- c("lng", "lat", "id", "cluster_id")
c_offpeak_dropoff <- cluster_offpeak_dropoff %>%
  group_by(cluster_id) %>%
  summarise_at(vars(id), n_distinct) %>%
  mutate(lng=kms_offpeak_dropoff$centers[,1], lat=kms_offpeak_dropoff$centers[,2])

kmeansMap_offpeak_dropoff <- leaflet(c_offpeak_dropoff) %>% 
  addTiles() %>% 
  addMarkers(lng=~lng, lat=~lat,
             popup=as.character(c_offpeak_dropoff$id),
             clusterOptions = markerClusterOptions())

kmeansMap_offpeak_dropoff
