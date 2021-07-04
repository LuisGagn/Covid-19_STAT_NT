library(shiny)
library(tidyverse)
library(DT)

# Dataframes

#cdg = Covid Global Data // Es descargado desde Google pesa 153 MB
#cgd<-read.csv("https://query.data.world/s/25w3mrdsnje6zupnls5pajt3ruf6in", header=TRUE, stringsAsFactors=FALSE);

cgd<- read.csv("COVID.csv")
cords<- read.csv("https://raw.githubusercontent.com/LuisGagn/DataF/main/covid_19_clean_complete.csv")
vacunacion<- read.csv("https://raw.githubusercontent.com/3dgiordano/covid-19-uy-vacc-data/main/data/Uruguay.csv")
ui <- fluidPage(
                titlePanel("COVID-19 | Analisis"),
                sidebarLayout(
                sidebarPanel(
                    dateInput("fecha", "Selecciona la Fecha",min = 2020-01-21, max=2021-07-01),
                    selectInput("duracion", "Elige si quieres ver la fecha o los datos hasta la fecha", c("Hasta","Fecha"))), 
            
                mainPanel(
                    tabsetPanel(
                        tabPanel("Mapamundi", plotOutput("mapa"), dataTableOutput("datam")),
                        tabPanel("Vacunacion",plotOutput("vac"))
                ))

                ))














server <- function(input, output, session) {
cordenadas<-reactive(cords%>%select(Country.Region, Lat, Long, Province.State))
    
positivos<- reactive(
            cgd %>% select(PEOPLE_POSITIVE_CASES_COUNT,PEOPLE_POSITIVE_NEW_CASES_COUNT,
                           CONTINENT_NAME,COUNTRY_SHORT_NAME,PROVINCE_STATE_NAME,REPORT_DATE)%>%
            mutate(COUNTRY_SHORT_NAME=recode(COUNTRY_SHORT_NAME,`United States`="US"))
)

tabla()<-reactive(merge(positivos(),cordenadas(),by=c("COUNTRY_SHORT_NAME","Country.Region")))
output$datam <- DT::renderDataTable(tabla())
}





shinyApp(ui, server)