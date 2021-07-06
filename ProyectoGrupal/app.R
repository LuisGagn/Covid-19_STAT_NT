# LIBRERIAS
# Shiny cosas
library(shiny)
library(shinydashboard)


# Datos mundiales (Para mapamundi)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

# Manejo Data
library(lubridate)
library(DT)
library(tidyverse)

# Graficos
library(scales)
library(ggplot2)
library(ggiraph)
library(dygraphs)
library(xts)

# DataSets || En caso de no descargar COVID.csv, comentar la fila 14 y descomentar la 13
#cgd<-read.csv("https://query.data.world/s/25w3mrdsnje6zupnls5pajt3ruf6in", header=TRUE, stringsAsFactors=FALSE);
cgd<- read.csv("COVID.csv")
vacunacion<- read.csv("https://raw.githubusercontent.com/3dgiordano/covid-19-uy-vacc-data/main/data/Uruguay.csv")
world <- ne_countries(scale = "medium", returnclass = "sf")
Datos<- cgd

##########
## ui.R ##
##########

#### Encabezado ####

encabezado <- dashboardHeader(title = "COVID-19 | ANALISIS")

#### Barra Lateral ####

barra_lateral <- dashboardSidebar(
  
  sidebarMenu(
              menuItem("Inicio", tabName = "inicio", icon = icon("home")),
              
              menuItem("Mapamundi", tabname="mapm", icon=icon("globe-americas"),
                       menuSubItem("Mapa",tabName = "mapita",icon=icon("plus-square")),
             
            # INPUTS MAPAMUNDI
             sliderInput("fecha", "Selecciona la Fecha", 
                         min=as.Date("2020-01-21", "%Y-%m-%d"),
                         max=as.Date("2021-07-01", "%Y-%m-%d"), 
                         value=as.Date("2020-02-20"), ticks = FALSE),
            
             selectInput("tipo", "Selecciona estilo", c("Diario","Total")),
             selectInput("tipo2", "Selecciona Datos", c("Muertes","Infectados"))
             ), # CLOSES MENUITEM MAPA
    
    menuItem("Linea Temporal Latam", tabName = "vcaa", icon = icon("chart-area"),
             menuSubItem("Ver", tabName = "ver", icon = icon("plus-square")),
             
            # INPUTS VACUNACION // ignorar es para ver como se hace todo    
             selectInput("va","Seleccione el Pais",
                        c("Uruguay","Argentina","Brasil","Chile")))
    
  ) # CLOSES SIDEBARMENU
  ) # CLOSES DASHBOARDSIDEBAR


#### Contenido ####
contenido <- dashboardBody(
  
# DASH VACUNACION || IGNORAR TESTEO / APRENDIZAJE  
  tabItems(
    tabItem(tabName = "inicio", h1("Introduccion")),
    tabItem(tabName = "ver", 
            tabsetPanel(
            tabPanel("Visualizacion", 
                    fluidRow(     
                    box(
                    title = "Analisis temporal", status = "danger", solidHeader = TRUE,width = 700, 
                    dygraphOutput("vacunas")
                    ) 
                    )
            )
            )
  ),
  
# DASH MAPA 
  tabItem(tabName = "mapita",
          tabsetPanel(
          tabPanel("Mapa",
                  fluidRow(
                  box(
                  title="Mapa Global", status="danger", solidHeader=TRUE, width = 1000, 
                  p("Pasa el mouse sobre un pais para saber la cantidad de casos/Muertes"),girafeOutput("mapa")
                  )
                  )
          ),
          
          tabPanel("Tabla",fluidRow(DT::dataTableOutput("tabla")))
          )
) # CLOSES TABITEM MAPA
) # CLOSES TAB ITEMS
) # CLOSES DASHBOARD BODY





## UI - Interfaz del Usuario ##

ui <- dashboardPage(skin = "black", encabezado, barra_lateral, contenido)





###########################






server <- function(input, output, session) {
  
  
  #OPTIMIZACION EN PROGRESO
  
  # SELECCION DE FECHA
  ###############
  cgd$REPORT_DATE<- as.Date(cgd$REPORT_DATE)
  
  casos2<-reactive(
    cgd %>%group_by(COUNTRY_SHORT_NAME) %>% filter(REPORT_DATE==input$fecha)
  )  
  
  # CAMBIO SEGUN DIARIO/TOTAL | INFECTADOS/MUERTES
  casos3<- reactive(if(input$tipo=="Total" && input$tipo2=="Infectados"){
    casos2() %>% 
      summarise("Casos"=sum(PEOPLE_POSITIVE_CASES_COUNT))  
    
  }else if(input$tipo=="Diario" && input$tipo2=="Infectados"){
    casos2() %>% 
      summarise("Casos"=sum(PEOPLE_POSITIVE_NEW_CASES_COUNT)) 
    
  }else if(input$tipo=="Total" && input$tipo2=="Muertes"){
    casos2() %>% 
      summarise("Casos"=sum(PEOPLE_DEATH_COUNT))
    
  }else{
    casos2() %>% 
      summarise("Casos"=sum(PEOPLE_DEATH_NEW_COUNT))
  }
  )
  
  # CAMBIO DE NOMBRES PARA EL MERGE
  casos<- reactive(casos3()%>% 
                     mutate( COUNTRY_SHORT_NAME=recode(COUNTRY_SHORT_NAME,
                                                       `United States`="United States of America", 
                                                       `Congo (Brazzaville)`="Republic of Congo",
                                                       `Congo (Kinshasa)`="Democratic Republic of the Congo",
                                                       `Tanzania`="United Republic of Tanzania")))
  
  # MERGE DATOS GLOBALES Y CASOS  
  COVID.world <- reactive(merge(world, casos(), by.x="admin", by.y="COUNTRY_SHORT_NAME"))
  
  # Mapamundi Grafico Paises
  
  graf2<-reactive(ggplot(COVID.world()) + 
                    geom_sf_interactive(aes(tooltip=Casos,data_id=sovereignt,fill=Casos),color = NA) +
                    scale_fill_gradient(label=comma) +
                    theme_void())
  
  graf<-reactive(
    if(input$tipo2=="Muertes"){
      graf2()+labs(fill="Muertes")+
        scale_fill_gradientn(colours = c("#f7e6e6","#bf3e3e","#340404"),
                             na.value = "grey50",label=comma
        )
    }else{
      graf2()+labs(fill="Infectados")+
        scale_fill_gradientn(colours = c("#f0ebd4","#b89d2c","#5c4e16"),
                             na.value = "grey50",label=comma
        )
    }
  )
  
  
  
  ######################################################
  ######################################################
  ######################################################
  ######################################################
  Datos<- cgd
  colnames(Datos) = c("PEOPLE_POSITIVE_CASES_COUNT", "COUNTY_NAME","PROVINCE_STATE_NAME","REPORT_DATE" ,"CONTINENT_NAME","DATA_SOURCE_NAME",
                      "PEOPLE_DEATH_NEW_COUNT","COUNTY_FIPS_NUMBER","COUNTRY_ALPHA_3_CODE",
                      "COUNTRY","COUNTRY_ALPHA_2_CODE","PEOPLE_POSITIVE_NEW_CASES_COUNT", "PEOPLE_DEATH_COUNT")
  
  Datos$COUNTRY <- factor(Datos$COUNTRY)
  Datos$REPORT_DATE <- as.Date(Datos$REPORT_DATE)
  
  datos_UY_Por_Fecha <-reactive( 
    
    if(input$va=="Uruguay"){
      Datos %>% filter(COUNTRY == "Uruguay") %>% 
        select(REPORT_DATE, starts_with("People_")) %>% 
        arrange(REPORT_DATE)
    }else if(input$va=="Brasil"){  
      Datos %>% filter(COUNTRY == "Brazil") %>% 
        select(REPORT_DATE, starts_with("People_")) %>% 
        arrange(REPORT_DATE)
    }else if(input$va=="Argentina"){  
      Datos %>% filter(COUNTRY == "Argentina") %>% 
        select(REPORT_DATE, starts_with("People_")) %>% 
        arrange(REPORT_DATE)
    }else{
      Datos %>% filter(COUNTRY == "Chile") %>% 
        select(REPORT_DATE, starts_with("People_")) %>% 
        arrange(REPORT_DATE)
      }
    
    )
    

  
  datos_UY_Por_Fecha_ts <- reactive(xts(x=datos_UY_Por_Fecha()[,3:5],
                               order.by = datos_UY_Por_Fecha()$REPORT_DATE))
 
  
  
  ######################################################
  ######################################################
  ######################################################
  
  
  
  
  
 
  #Calls
  output$mapa<-renderGirafe(girafe(ggobj = graf(),width_svg = 7, height_svg = 4, 
                                   options=list(opts_sizing(rescale=TRUE), 
                                                opts_hover(css = "fill:#C7B6DC;stroke:#C7B6DC;cursor:pointer;"))))
  
  
  
  output$tabla <- DT::renderDataTable({casos()})
  
  output$vacunas<- renderDygraph({dygraph(datos_UY_Por_Fecha_ts()) %>% 
      dyOptions(labelsUTC = TRUE, labelsKMB = TRUE,
                fillGraph = TRUE, fillAlpha = 0.05,
                drawGrid = FALSE) %>% dyRangeSelector() %>% 
      dyCrosshair(direction = "vertical")%>%
      dyLegend(show="always",width = 400)})

}





shinyApp(ui, server)