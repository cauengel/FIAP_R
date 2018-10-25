# Função check.packages: instala vários pacotes do R
# Verifica se os pacotes estão instalados e instala os que não estiverem
check.packages <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
}

# Seleciona os pacotes desejados
packages <- c("dplyr", 
              "lubridate", 
              "plotly", 
              "stringr", 
              "ggplot2", 
              "ggmap", 
              "magrittr", 
              "gbm",
              "leaflet",
              "shiny",
              "DT")

# Chama a função com os pacotes desejadas
check.packages(packages)