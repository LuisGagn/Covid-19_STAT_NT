# LIBRERIAS
library(shiny)
library(tidyverse)
library(DT)
library(scales)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggplot2)
library(lubridate)

# DataSets || En caso de no descargar COVID.csv, comentar la fila 14 y descomentar la 13
#cgd<-read.csv("https://query.data.world/s/25w3mrdsnje6zupnls5pajt3ruf6in", header=TRUE, stringsAsFactors=FALSE);
cgd<- read.csv("COVID.csv")
vacunacion<- read.csv("https://raw.githubusercontent.com/3dgiordano/covid-19-uy-vacc-data/main/data/Uruguay.csv")

#UI SHINY
ui <- fluidPage(
  titlePanel("COVID-19 | Analisis"),
  
  tabsetPanel(
    tabPanel("Global",h1("Casos Globales"),
             sidebarLayout(
               sidebarPanel(dateInput("fecha", "Selecciona la Fecha",min = "2020-01-21", max="2021-07-01", value="2020-02-20")),mainPanel(
                 plotOutput("mapa"),DT::dataTableOutput("tabla")
               ))),    
    
    tabPanel("America",h1("Test"),sidebarLayout(
               sidebarPanel(dateInput("fecha", "Selecciona la Fecha",min = "2020-01-21", max="2021-07-01", value="2020-02-20")),mainPanel(
                 
               )))
))













server <- function(input, output, session) {
  
  cgd$REPORT_DATE<- as.Date(cgd$REPORT_DATE)
 
#Tabla de casos totales reducida
   casos<-reactive(
          cgd %>%group_by(COUNTRY_SHORT_NAME) %>% filter(REPORT_DATE==input$fecha)%>% 
          summarise("Casos"=sum(PEOPLE_POSITIVE_CASES_COUNT))%>% 
          mutate( COUNTRY_SHORT_NAME=recode(COUNTRY_SHORT_NAME,
                                            `United States`="United States of America", 
                                            `Congo (Brazzaville)`="Republic of Congo",
                                            `Congo (Kinshasa)`="Democratic Republic of the Congo",
                                            `Tanzania`="United Republic of Tanzania"))
    )

# Datos Paises para su ploteo
    world <- ne_countries(scale = "medium", returnclass = "sf")
    COVID.world <- reactive(merge(world, casos(), by.x="admin", by.y="COUNTRY_SHORT_NAME"))
  
# Mapamundi Grafico
    graf<-reactive(ggplot(COVID.world()) + 
                   geom_sf(aes(fill=Casos)) +
                   scale_fill_gradient(label=comma) +
                   theme_classic()
                   )
  
  
    
#Calls
  output$mapa<-renderPlot(graf())
  output$tabla <- DT::renderDataTable({casos()})

}





shinyApp(ui, server)