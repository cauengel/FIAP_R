#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)
library(stringr)
library(DT)
library(lubridate)
library(dplyr)
library(leaflet)

###################
##   Shiny UI    ##
###################
# Define UI for application
ui <- fluidPage(
  # Application title
  titlePanel("Trabalho Final - Análise de Corridas de Táxi de NY"),
  navbarPage("",
             tabPanel("Dados de Entrada",
                      h3("Fontes de dados"),
                      p("As corridas de táxi para todas as análises, exceto regressões, tiveram os seus dados obtidos a partir do arquivo train.csv fornecido pelo professor"),  
                      p("Os dados originais sobre as corridas eram : "),
                      br(),
                      tags$ul(
                        tags$li("id – chave única de cada corrida"),
                        tags$li("vendor_id – Código do provedor da informação"),
                        tags$li("pickup_datetime – Hora que a corrida se iniciou"),
                        tags$li("dropoff_datetime – Hora que a corrida se encerrou"),
                        tags$li("passenger_count – Quantidade de passageiros na corrida"),
                        tags$li("pickup_longitude – Longitude do início da corrida"),
                        tags$li("pickup_latitude – Latitude do início da corrida"),
                        tags$li("dropoff_longitude – Longitude do final da corrida"),
                        tags$li("dropoff_latitude – Latitude do final da corrida")
                      ),
                      br(),
                      p("Os dados de chuva, temperatura e neve foram adicionados a partir de uma fonte de dados externa (csv) contendo as informações 
                        das condições climáticas de NY em 2016."),
                      p("Essas informações sobre o clima foram obtidas a partir do site kaggle na url: 
                        https://www.kaggle.com/mathijs/weather-data-in-new-york-city-2016."),
                      br(),
                      p("Neste arquivo constavam as seguintes informações:"),
                      br(),
                      tags$ul(
                        tags$li("date – Data do registro metereológico"),
                        tags$li("maximum.temperature – Temperatura máxima em Fahrenheit ocorrida no dia"),
                        tags$li("minimum.temperature – Temperatura mínima em Fahrenheit ocorrida no dia"),
                        tags$li("average.temperature – Média da temperatura em Fahrenheit ocorrida no dia"),
                        tags$li("precipitation – Precipitação de chuva ocorrida durante o dia"),
                        tags$li("snow.fall – Precipitação de neve ocorrida durante o dia"),
                        tags$li("snow.depth – Profundidade da neve depositada durante o dia")
                      ),
                      br(),
                      p("O arquivo de corridas de teste para as regressões, test.csv, também fornecido pelo professor, contém a mesma estrutura que o arquivo train.csv com exceção dos campos dropoff_datetime e trip_duration"),
                      br()
                      ),
             tabPanel("Filtros",
                      h3("Corridas de Taxi"),
                      p("As corridas de táxi sofreram os seguintes filtros : "),  
                      br(),
                      tags$ul(
                        tags$li("Outliers – foram retiradas todas as corridas cujas coordenadas(origem,destino) eram classificadas como outliers pela função boxplot"),
                        tags$li("Limites da cidade – foram eliminadas as corridas cujas coordenadas(origem,destino) excediam os limites da cidade"),
                        tags$li("Duração da viagem – as corridas cuja duração eram menores que 1 minuto foram excluídas também.")
                      ),
                      br(),
                      p("Esses filtros se aplicam tanto para as corridas do dataset train.csv como para aquelas do dataset test.csv."),
                      br(),
                      h3("Clima"),
                      p("Para o clima não houve necessidade de filtro dado que todas as informações são referentes ao ano de 2016")
                      ),
                          
             tabPanel("Enriquecimento",
                      h3("Enriquecimento dos dados"),
                      p("Para a análise de corridas de NY, foram adicionadas as seguintes colunas ao dados de treinamento:"),               
                      tags$ul(
                        tags$li("dist_geo_km - Distância geométrica em km"),
                        tags$li("dist_man_km - Distância manhattan em km"),
                        tags$li("posicao_coluna_matriz_origem - Índice da coluna na matriz de quadrantes da origem"),
                        tags$li("posicao_linha_matriz_origem - Índice da linha na matriz de quadrantes da origem"),
                        tags$li("posicao_coluna_matriz_destino - Índice da coluna na matriz de quadrantes do destino"),
                        tags$li("posicao_linha_matriz_destino - Ìndice da linha na matriz de quadrantes do destino"),
                        tags$li("dia_mes_inicio - Dia do mês do início da corrida"),
                        tags$li("mes_inicio - Mês do início da corrida"),
                        tags$li("dia_semana_inicio - Dia da semana do início da corrida"),
                        tags$li("hora_inicio - Hora do início da corrida"),
                        tags$li("dia_mes_fim - Dia do mês do término da corrida"),
                        tags$li("mes_fim - Mês do término da corrida"),
                        tags$li("dia_semana_fim - Dia da semana do término da corrida"),
                        tags$li("hora_fim - Hora do término da corrida"),
                        tags$li("prep_chuva - Indica a precipitação de chuva no dia da corrida"),
                        tags$li("chovendo - Indica se choveu no dia da corrida"),
                        tags$li("prep_neve - Indica a precipitação de neve no dia da corrida"),
                        tags$li("nevando - Indica se nevou no dia da corrida"),
                        tags$li("temp_media_celsius - Indica a temperatura em celsius no dia da corrida"),
                        tags$li("horario_pico - Indica se a corrida ocorreu em horário de pico"),
                        tags$li("fim_de_semana - Indica se a corrida iniciou no fim de semana")
                      ),
                      br(),
                      p("Os quadrantes da posição de início e fim da corrida foram calculados utilizando quadrantes de 14 x 14 metros."),
                      p("A partir dessa discretização do mapa de NY, gerou-se a matriz 'matriz_deslocamento' contendo a quantidade de vezes que um táxi passa por aquele quadrante."),
                      p("Nessa matriz constam a quantidade de vezes que um táxi passou por lá, supondo que a sua trajetória da origem ao destino é uma reta passando pelos quadrantes dessa matriz."),
                      p("Para o cálculo dessa matriz, utilizam-se as equações de retas representando as linhas da matriz com as linhas das trajetórias dos taxis, por fim, essa matriz é utilizada para gerar o mapa de calor."),
                      br(),
                      "Tabela de corridas após enriquecimento dos dados",
                      br(),
                      DT::dataTableOutput("tabela_enriquecida")
                      ),
             navbarMenu("Analise Exploratória",
                        tabPanel("Entradas e Saidas",
                                 h3("Análise exploratória dos embarques e desembarques"),
                                 p("Na imagem abaixo são ilustradas todas os origem das corridas dos dados analisados"),
                                 imageOutput("saidas_taxi", height = 480),
                                 p("Na imagem abaixo são ilustradas todos os destinos das corridas dos dados analisados"),
                                 imageOutput("chegadas_taxi", height = 480),
                                 br(),br(),
                                 p("Abaixo a relação da quantidade de corridas em função do dia da semana. Nota-se uma maior quantidade de corridas na Sexta e no Sábado diminuindo consideravelmente no Domingo e na Segunda."),
                                 DT::dataTableOutput("tabela_corridas_dia_semana"),
                                 br(),br(),
                                 p("Na próxima tabela, verifica-se as principais corridas em função do dia do ano."),
                                 p("As 9 principais corridas foram realizadas no Sábado e a décima na Sexta-feira."),
                                 DT::dataTableOutput("tabela_corridas_dia_ano"),
                                 br(),br(),
                                 p("Abaixo um tabela com os principais horários de corridas, sendo que a maioria das corridas ocorrem entre 18 e 22 horas."),
                                 DT::dataTableOutput("tabela_corridas_hora_dia"),
                                 br(),br(),
                                 p("Abaixo são indicadas as principais origens em função do horário"),
                                 DT::dataTableOutput("tabela_corridas_por_origem"),
                                 p("Principal ponto de origem:"),
                                 leafletOutput("mapa_principal_origem"),
                                 br(),br(),
                                 p("Abaixo são indicados os principais destinos em função do horario"),
                                 DT::dataTableOutput("tabela_corridas_por_destino"),
                                 p("Principal ponto de destino:"),
                                 leafletOutput("mapa_principal_destino")
                        ),
                        tabPanel("Pontos de Interesse",
                                 h3("Análise exploratória dos pontos de interesse"),
                                 p("Na imagem estão ilustrados os pontos de interesse utilizados nas análises"),
                                 leafletOutput("mapa_pontos_interesses"),
                                 DT::dataTableOutput("tabela_pontos_interesses"),
                                 br(), br(),
                                 p("A tabela abaixo indica a classificação dos horários com mais saídas por dia da semana e ponto de origem"),
                                 fluidRow(
                                   column(4,
                                          selectInput("Origem", 
                                                      label = "Selecione um ponto de Origem",
                                                      choices = pontos_interesse$Ponto),
                                          checkboxGroupInput("DiasSemana", 
                                                             "Dias", 
                                                             choices = list("Domingo" = 1,
                                                                            "Segunda" = 2, 
                                                                            "Terca"   = 3, 
                                                                            "Quarta"  = 4,
                                                                            "Quinta"  = 5,
                                                                            "Sexta"   = 6,
                                                                            "Sabado"  = 7),
                                                             selected = 1:7)
                                   ),
                                   column(6,
                                          DT::dataTableOutput("tabela_classificacao_origem")
                                   )
                                 ),
                                 br(), br(),
                                 p("A tabela abaixo indica a classificação dos horários por dia da semana e ponto de destino"),
                                 fluidRow(
                                   column(4,
                                          selectInput("Destino", 
                                                      label = "Selecione um ponto de Destino",
                                                      choices = pontos_interesse$Ponto),
                                          checkboxGroupInput("DiasSemana", 
                                                             "Dias", 
                                                             choices = list("Domingo" = 1,
                                                                            "Segunda" = 2, 
                                                                            "Terca"   = 3, 
                                                                            "Quarta"  = 4,
                                                                            "Quinta"  = 5,
                                                                            "Sexta"   = 6,
                                                                            "Sabado"  = 7),
                                                             selected = 1:7)
                                   ),
                                   column(6,
                                          DT::dataTableOutput("tabela_classificacao_destino")
                                   )
                                 )
                        )
             ),
             navbarMenu("Análise Gráfica",
                        tabPanel("Corridas por Mês",
                                 h3("Análise gráfica - Corridas por mês"),
                                 plotlyOutput("graf_corridas_por_mes"),
                                 p("O gráfico acima ilustra a quantidade total de corridas realizadas em NY no primeiro semestre de 2016."),
                                 p("Pode-se notar que há um aumento gradual no número de corridas a partir de Janeiro, com pico em Março, e posteriormente
                                   diminuindo até aproximadamente 200 mil corridas em Junho."),
                                 p("Essa tendência possivelmente deve-se a proximidade de Janeiro e Junho com as férias de inverno e verão.
                                   O pico em Março pode ter ocorrido por eventos na cidade como shows e o St Patrick's Day")
                                 ),
                        tabPanel("Corridas por Dia da Semana",
                                 h3("Análise gráfica - Corridas por dia da semana"),                   
                                 plotlyOutput("graf_corridas_por_dia_semana"),
                                 p("Neste gráfico são indicados o número total de corridas por dia da semana no primeiro semestre de 2016."),
                                 p("Nota-se um menor número de corridas no Domingo e Segunda-feira, crescendo a partir de Terça-feira e com pico no Sábado."),
                                 p(textOutput("corrida_dia_semana_analise"))
                        ),
                        tabPanel("Corridas por Quarto de Hora",
                                 h3("Análise gráfica - Corridas por dia da semana e quarto de hora"),
                                 sidebarPanel(
                                   checkboxGroupInput("QuartoHoraDiasSemana", 
                                                      "Dias", 
                                                      choices = list("Domingo" = 1,
                                                                     "Segunda" = 2, 
                                                                     "Terca"   = 3, 
                                                                     "Quarta"  = 4,
                                                                     "Quinta"  = 5,
                                                                     "Sexta"   = 6,
                                                                     "Sabado"  = 7),
                                                      selected = 1:7)
                                 ),
                                 mainPanel(
                                   plotlyOutput("graf_corridas_por_quarto_hora"),
                                   p("Neste gráfico é ilustrado a quantidade de corridas por quarto de hora no primeiro semestre de 2016."),
                                   p("De Segunda a Sexta, nota-se um aumento na demanda a partir das 5 horas e 15 minutos diminuindo a partir das 8 horas e 45 minutos e posteriormente se estabilizando. A partir das 16 horas e 30 minutos a demanda volta a crescer até as 19 horas, quando passa a diminuir novamente."),
                                   p("Durante o fim de semana há uma tendência em aumentar a demanda a partir das 6 horas, estabilizando-se por volta das 11 horas."),
                                   p("Nas Sextas e Sábados, dias com maiores quantidade de corridas, a demanda tende a crescer a partir das 5 horas e 15 minutos, uma ligeira variação durante a tarde e um aumento a partir das 18 horas. Nota-se que a demanda continua alta durante a noite.")
                                 )
                        ),
                        tabPanel("Clustering",
                                 h3("Análise gráfica - Clustering"),
                                 p("Os gráficos abaixo ilustram os principais agrupamentos de origem e destino em horário de pico e fora do horário de pico."),
                                 p("Para os horários de picos foram consideradas as corridas entre 7 e 9 horas (7:00 a 9:59) e 17 a 19 horas (17:00 a 19:00."),
                                 h4("Cluster de embarque em horário de pico"),
                                 leafletOutput("cluster_embarque_pico"),
                                 p("Percebe-se pelo cluster que as principais saídas em horário de pico estão concentradas em:"),
                                 tags$ul(
                                   tags$li("Região do World Trade Center / Wall Street"),
                                   tags$li("Região da NYU"),
                                   tags$li("Região de Chelsea"),
                                   tags$li("Região de Kips Bay"),
                                   tags$li("Região da Broadway"),
                                   tags$li("Região próxima ao Grand Central Station"),
                                   tags$li("Região de Lenox Hill"),
                                   tags$li("Região de Upper West Side próxima ao Museu de Historia Natural"),
                                   tags$li("Região de Upper East Side"),
                                   tags$li("Região de Upper West Side próxima a Universidade de Columbia")
                                 ),
                                 p(),
                                 h4("Cluster de desembarque em horário de pico"),
                                 leafletOutput("cluster_desembarque_pico"),
                                 p("Percebe-se pelo cluster que os principais destinos em horário de pico estão concentrados em:"),
                                 tags$ul(
                                   tags$li("Região do World Trade Center / Wall Street"),
                                   tags$li("Região da NYU"),
                                   tags$li("Região de Chelsea"),
                                   tags$li("Região de Kips Bay"),
                                   tags$li("Região da Broadway"),
                                   tags$li("Região próxima ao Grand Central Station"),
                                   tags$li("Região de Lenox Hill"),
                                   tags$li("Região de Upper West Side próxima ao Museu de Historia Natural"),
                                   tags$li("Região do sul de Harlem"),
                                   tags$li("Região de Wiiliamsburg no Brooklyn")
                                 ),
                                 p(),
                                 h4("Cluster de embarque fora do horário de pico"),
                                 leafletOutput("cluster_embarque_fora_pico"),
                                 p("Percebe-se pelo cluster que, mesmo fora do horário de pico, os clusters mantêm-se nas mesmas posições dentro do horário de pico"),
                                 p(),
                                 h4("Cluster de desembarque fora do horário de pico"),
                                 leafletOutput("cluster_desembarque_fora_pico"),
                                 p("Percebe-se pelo cluster que, mesmo fora do horário de pico, os clusters mantêm-se nas mesmas posições dentro do horário de pico")
                        ),
                        tabPanel("Mapa de calor",
                                 h3("Analise gráfica - Mapa de Calor"),
                                 plotlyOutput("graf_calor"),
                                 p("O mapa de calor ilustra as coordenadas por onde as corridas ocorreram dentro de uma amostra com 100.000 elementos."),
                                 p("Pode-se notar que a maioria dos trajetos ocorreram no centro de Manhattan.")
                        )
                                 ),
             navbarMenu("Machine Learning",
                        tabPanel("Modelo",
                                 imageOutput("modelo_machine_learning")
                        ),
                        tabPanel("Regressão",
                                 h3("Regressões"),
                                 p("O objetivo da regresssão é estimar o tempo de duração da viagem para os dados de teste (test.csv)."),
                                 p("Primeiramente, três regressões serão avaliadas utilizando os dados de treinamento e o melhor algoritmo será utilizado para estimar o tempo dos dados de teste."),
                                 p(),
                                 h4("Regressão Linear Simples"),
                                 p("Prevê o tempo de duração baseado na relação entre o tempo real e a distância geométrica por meio de um modelo linear"),
                                 plotOutput("regressao_linear_simples_grafico"),
                                 verbatimTextOutput("regressao_linear_simples_sumario"),
                                 p(),
                                 h4("Regressão Linear Múltipla"),
                                 p("Prevê o tempo de duração baseado na relação entre o tempo real e todos os outros dados das corridas por meio de um modelo linear. Além disso, utiliza as flags horário de pico e fim de semana como fator."),
                                 plotOutput("regressao_linear_multipla_grafico"),
                                 verbatimTextOutput("regressao_linear_multipla_sumario"),
                                 p(),
                                 h4("Boosted Regression Tree"),
                                 p("Prevê o tempo de duração baseado na relação entre o tempo real e todos os outros dados das corridas por meio de um modelo não-linear. Além disso, utiliza as flags horário de pico e fim de semana como fator."),
                                 plotOutput("boosted_regression_grafico"),
                                 p(),
                                 h4("Comparando as regressões"),
                                 p("Para avaliar as regressões foram utilizados 3 métodos:"),
                                 tags$ul(
                                   tags$li("Root Mean Squared Error (RSME)"),
                                   tags$li("Mean Absolute Error (MAE)"),
                                   tags$li("R-quadrado (R^2)")
                                 ),
                                 p("Os resultados obtidos foram:"),
                                 tableOutput("tabela_comparacao_regressao"),
                                 p("Assim, pode-se concluir que o Boosted Regression Tree apresenta o melhor resultado, pois possui os menores índices de erro (RSME e MAE) e um maior coeficiente de determinação (R^2)"),
                                 h4("Aplicando a regressão aos dados de teste"),
                                 p("Aplicando o Boosted Regression Tree aos dados de teste foram obtidos os dados da tabela abaixo:"),
                                 DT::dataTableOutput("tabela_regressao_previsao")
                        )
             ),
             tabPanel("Grupo",
                      tags$ul(
                        tags$li("Cauê Engelmann - RM 331199"),
                        tags$li("Marcelo Gulfier - RM 330738"),
                        tags$li("Marcos Massaharu Muto - RM 330930"),
                        tags$li("Priscila Daniele Fritsch Gonçalves - RM 331893")
                      )                  
                      )
             )
             
  
  )

########################
##    Shiny Server    ##
########################
# Define server logic 
server <- function(input, output) {
  
  output$tabela_enriquecida = DT::renderDataTable({
    corridas.taxis.ny.enriq
  })
  
  output$tabela_corridas_dia_semana = DT::renderDataTable({
    viagens_por_dia_semana %>% select(dia, total_viagens) %>% arrange(desc(total_viagens))
  }, options = list(dom = 't'), rownames = FALSE, colnames = c("Dia da Semana", "Quantidade Corridas"))
  
  output$corrida_dia_semana_analise = renderText({
    paste(c("Ressalta-se no gráfico uma diferença de ", 
            format((viagens_por_dia_semana %>% filter(dia_semana_inicio == 7) %>% select(total_viagens) -
                      viagens_por_dia_semana %>% filter(dia_semana_inicio == 2) %>% select(total_viagens)) / 
                     viagens_por_dia_semana %>% filter(dia_semana_inicio == 2) %>% select(total_viagens) * 100, digits = 4, decimal_mark = ","),
            "% de corridas entre Sábado e Segunda, e um aumento de ",
            format((viagens_por_dia_semana %>% filter(dia_semana_inicio == 3) %>% select(total_viagens) -
                      viagens_por_dia_semana %>% filter(dia_semana_inicio == 2) %>% select(total_viagens)) / 
                     viagens_por_dia_semana %>% filter(dia_semana_inicio == 2) %>% select(total_viagens) * 100, digits = 4, decimal_mark = ","),
            "% entre Segunda e Terça-feira."), sep = "")
  })
  
  output$tabela_corridas_dia_ano = DT::renderDataTable({
    corridas.taxis.ny.enriq %>% select(dia_mes_inicio, mes_inicio) %>% group_by(dia_mes_inicio, mes_inicio) %>% summarise('total' = n()) %>% arrange(desc(total))
  }, options = list(dom = 't'), colnames = c("Dia do Mes", "Mes", "Quantidade Corridas"))
  
  output$tabela_corridas_hora_dia = DT::renderDataTable({
    principais_horarios
  }, options = list(dom = 't'), colnames = c("Hora do Dia", "Quantidade Corridas"))
  
  output$tabela_corridas_por_origem = DT::renderDataTable({
    principais_origens_horario
  }, options = list(dom = 't'), colnames = c("Coluna", "Linha", "Hora", "Quantidade Corridas"))
  
  output$mapa_principal_origem <- renderLeaflet({
    leaflet(corridas.taxis.ny.enriq %>% filter(
      posicao_coluna_matriz_origem == (principais_origens_horario %>% head(1))$posicao_coluna_matriz_origem,
      posicao_linha_matriz_origem == (principais_origens_horario %>% head(1))$posicao_linha_matriz_origem) %>%
        select(pickup_longitude, pickup_latitude) %>% head(1)) %>% 
      addTiles() %>% 
      addCircleMarkers(lng=~pickup_longitude, lat=~pickup_latitude,
                       clusterOptions = markerClusterOptions())
  })
  
  output$tabela_corridas_por_destino = DT::renderDataTable({
    principais_destinos_horario
  }, options = list(dom = 't'), colnames = c("Coluna", "Linha", "Hora", "Quantidade Corridas"))
  
  output$mapa_principal_destino <- renderLeaflet({
    leaflet(corridas.taxis.ny.enriq %>% filter(
      posicao_coluna_matriz_destino == (principais_destinos_horario %>% head(1))$posicao_coluna_matriz_destino,
      posicao_linha_matriz_destino == (principais_destinos_horario %>% head(1))$posicao_linha_matriz_destino) %>%
        select(dropoff_longitude, dropoff_latitude) %>% head(1)) %>% 
      addTiles() %>% 
      addCircleMarkers(lng=~dropoff_longitude, lat=~dropoff_latitude,
                       clusterOptions = markerClusterOptions())
  })
  
  output$graf_calor <- renderPlotly({
    cores.taxi  = c("blue", "#63be7b", "#ffeb84",  "#f8696b")
    gradiente.taxi = colorRampPalette(cores.taxi)(n = 30)
    plot_ly(x = longitudes, y = latitudes, z=matriz_deslocamento,colors=gradiente.taxi,type="heatmap") %>% 
      layout(title = "Mapa de calor", xaxis = list(title = "Longitude"), yaxis = list(title = "Latitude"))
  })
  
  output$graf_corridas_por_mes <- renderPlotly({
    plot_ly(viagens_por_mes, x = ~mes, y = ~total_viagens, type = 'scatter', mode = 'lines') %>% 
      layout(title = "Corridas por mes", xaxis = list(title = "Mes"), yaxis = list(title = "Quantidade Corridas"))
  })
  
  output$graf_corridas_por_dia_semana <- renderPlotly({
    plot_ly(viagens_por_dia_semana, x = ~dia, y = ~total_viagens, type = 'scatter', mode = 'lines') %>% 
      layout(title = "Corridas por dia da semana", xaxis = list(title = "Dia da Semana"), yaxis = list(title = "Quantidade Corridas"))
  })
  
  output$graf_corridas_por_quarto_hora <- renderPlotly({
    plot_ly(viagens_dia_semana_quarto_hora %>% filter(dia_semana_inicio %in% input$QuartoHoraDiasSemana) %>%
              group_by(horario) %>% summarise(total=sum(total_viagens)), 
            x = ~horario, y = ~total, type = 'scatter', mode = 'lines') %>% 
      layout(title = "Corridas por quarto de hora", xaxis = list(title = "Horario"), yaxis = list(title = "Quantidade Corridas"))
  })
  
  output$tabela_classificacao_origem = DT::renderDataTable({
    exp_tabela_origem %>% filter(Origem == input$Origem, DiasSemana %in% input$DiasSemana) %>%
      group_by(Horario) %>% summarise(Total=n()) %>% arrange(desc(Total))
  }, options = list(dom = 't'), colnames = c("Hora", "Total Corridas"))
  
  output$tabela_classificacao_destino = DT::renderDataTable({
    exp_tabela_destino %>% filter(Destino == input$Destino, DiasSemana %in% input$DiasSemana) %>%
      group_by(Horario) %>% summarise(Total=n()) %>% arrange(desc(Total))
  }, options = list(dom = 't'), colnames = c("Hora", "Total Corridas"))
  
  output$saidas_taxi = renderImage({
    list(src = nome_arq_png_saidas_completo ,
         width = 480,
         height = 480,
         alt = "Saidas de taxi")
  },deleteFile = FALSE)
  
  output$chegadas_taxi = renderImage({
    list(src = nome_arq_png_chegadas_completo ,
         width = 480,
         height = 480,
         alt = "Chegadas de taxi")
  },deleteFile = FALSE)
  
  output$cluster_embarque_pico <- renderLeaflet({
    kmeansMap_peak_pickup
  })
  
  output$cluster_desembarque_pico <- renderLeaflet({
    kmeansMap_peak_dropoff
  })
  
  output$cluster_embarque_fora_pico <- renderLeaflet({
    kmeansMap_offpeak_pickup
  })
  
  output$cluster_desembarque_fora_pico <- renderLeaflet({
    kmeansMap_offpeak_dropoff
  })
  
  output$mapa_pontos_interesses <- renderLeaflet({
    leaflet(pontos_interesse) %>% 
      addTiles() %>% 
      addCircleMarkers(lng=~as.numeric(as.character(Longitude)), lat=~as.numeric(as.character(Latitude)),
                       popup=as.character(pontos_interesse$Ponto),
                       clusterOptions = markerClusterOptions())
  })
  

  output$tabela_pontos_interesses = DT::renderDataTable({
    pontos_interesse %>% select(Ponto)
  }, rownames = FALSE)
  
    
  output$modelo_machine_learning = renderImage({
    list(src = nome_arq_png_modelo_machine_learning ,
         alt = "Modelo Machine Learning")
  },deleteFile = FALSE)
  
  output$regressao_linear_simples_grafico <- renderPlot({
    plt_linear.s + coord_fixed(xlim = c(0,3000), ylim = c(0,3000)) + geom_abline(color = "red", slope = 1)
  })
  
  output$regressao_linear_simples_sumario <- renderPrint({
    summary(regressor_linear.s)
  })
  
  output$regressao_linear_multipla_grafico <- renderPlot({
    plt_linear.m + coord_fixed(xlim = c(0,3000), ylim = c(0,3000)) + geom_abline(color = "red", slope = 1)
  })
  
  output$regressao_linear_multipla_sumario <- renderPrint({
    summary(regressor_linear.m)
  })
  
  output$boosted_regression_grafico <- renderPlot({
    plt_boosted + coord_fixed(xlim = c(0,3000), ylim = c(0,3000)) + geom_abline(color = "red", slope = 1)
  })
  
  output$tabela_comparacao_regressao <- renderTable({
    matrix(c(rmse_linear.s, rmse_linear.m, rmse_boosted,mae_linear.s,mae_linear.m,mae_boosted,r2_linear.s,r2_linear.m,r2_boosted), 
           ncol = 3 , dimnames = list(c("Regressao Linear Simples", "Regressao Linear Multipla", "Boosted Regression Tree"),c("RSME", "MAE", "R^2")))
  }, rownames = TRUE)
  
  output$tabela_regressao_previsao = DT::renderDataTable({
    corridas.taxis.ny.teste.previsao
  }, options = list(dom = 't'), rownames = FALSE)
  
}

# Run the application 
shinyApp(ui = ui, server = server)

