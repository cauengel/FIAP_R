# Fun��o check.packages: instala v�rios pacotes do R
# Verifica se os pacotes est�o instalados e instala os que n�o estiverem
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

# Chama a fun��o com os pacotes desejadas
check.packages(packages)