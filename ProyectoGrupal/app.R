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


#Datos<-read.csv("https://query.data.world/s/25w3mrdsnje6zupnls5pajt3ruf6in", header=TRUE, stringsAsFactors=FALSE);     # DATOS ACTUALIZABLES DIA A DIA
Datos<- read.csv("COVID.csv")
vacunacion<- read.csv("https://raw.githubusercontent.com/3dgiordano/covid-19-uy-vacc-data/main/data/Uruguay.csv")
world <- ne_countries(scale = "medium", returnclass = "sf")


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
    
    menuItem("Lineas Temporales", tabName = "vcaa", icon = icon("chart-area"),
             menuSubItem("Ver", tabName = "ver", icon = icon("plus-square")),
             
            # INPUTS VACUNACION // ignorar es para ver como se hace todo    
             selectInput("va","Seleccione el Pais",
                        c("Uruguay","Argentina","Brazil","Chile")))
    
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
                        title = "Analisis temporal", status = "primary", solidHeader = TRUE,width = 700, 
                        dygraphOutput("vacunas")
                    ) 
                    )
              ),
              tabPanel("Picos", 
                     fluidRow(
                     box(
                         title = "Maximo de Casos",status="warning",solidHeader = TRUE,width=350,
                         plotOutput("test33")
                     ),
                     box(title = "Maximo de Muertes",status="danger",solidHeader = TRUE,width=350,
                         plotOutput("test32")
                       
                     )
                     )
              ),
              tabPanel("Vacunacion",
                       fluidRow(
                         box(
                           title = "Efectos de la Vacunacion",status="success",solidHeader = TRUE,width=350,
                           plotOutput("vacunacion2")
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
                  title="Mapa Global", status="success", solidHeader=TRUE, width = 1000, 
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

ui <- dashboardPage(skin = "purple", encabezado, barra_lateral, contenido)





###########################






server <- function(input, output, session) {
  
  
  #OPTIMIZACION EN PROGRESO

  Datos$REPORT_DATE<- as.Date(Datos$REPORT_DATE)
  colnames(Datos) = c("PEOPLE_POSITIVE_CASES_COUNT", "COUNTY_NAME","PROVINCE_STATE_NAME","REPORT_DATE" ,"CONTINENT_NAME","DATA_SOURCE_NAME",
                      "PEOPLE_DEATH_NEW_COUNT","COUNTY_FIPS_NUMBER","COUNTRY_ALPHA_3_CODE",
                      "COUNTRY","COUNTRY_ALPHA_2_CODE","PEOPLE_POSITIVE_NEW_CASES_COUNT", "PEOPLE_DEATH_COUNT")
  
  Datos$COUNTRY <- factor(Datos$COUNTRY)
  
  
  
  
  # Date selector
  casos2<-reactive(
    Datos %>%group_by(COUNTRY) %>% filter(REPORT_DATE==input$fecha)
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
  
  # CAMBIO DE NOMBRES PARA EL MERGE DE PAISES CON DISTINTO NOMBRE
  casos<- reactive(casos3()%>% 
                     mutate( COUNTRY=recode(COUNTRY,
                                                       `United States`="United States of America", 
                                                       `Congo (Brazzaville)`="Republic of Congo",
                                                       `Congo (Kinshasa)`="Democratic Republic of the Congo",
                                                       `Tanzania`="United Republic of Tanzania",
                                                        `Cote d'Ivoire`="Ivory Coast"    )))
  
  # MERGE DATOS GLOBALES Y CASOS  
  COVID.world <- reactive(merge(world, casos(), by.x="admin", by.y="COUNTRY"))
  
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
  
  
  
  
  #####  #####  #####  #####  #####  #####  #####  #####  #####  #####  #####    
# DYGRAPH


  #Datos$REPORT_DATE <- as.Date(Datos$REPORT_DATE)
  
  datos_Por_Fecha <-reactive( 
    
    Datos %>% filter(COUNTRY == input$va) %>% 
      select(REPORT_DATE, starts_with("People_")) %>% 
      arrange(REPORT_DATE)
    )
    

  
  datos_Por_Fecha_ts <- reactive(xts(x=datos_Por_Fecha()[,3:5],
                               order.by = datos_Por_Fecha()$REPORT_DATE))
 
  
  dyg<-reactive({dygraph(datos_Por_Fecha_ts()) %>% 
      dyOptions(labelsUTC = TRUE, labelsKMB = TRUE,
                fillGraph = TRUE, fillAlpha = 0.05,
                drawGrid = FALSE) %>% dyRangeSelector() %>% 
      dyCrosshair(direction = "vertical")%>%
      dyLegend(show="always",width = 400)})
  
  
##### #####  #####  #####  #####  #####  #####  #####  #####  #####  #####  #####   

  test<-reactive(
    Datos %>% filter(COUNTRY==input$va)
   )
  
  maximo<-reactive(
    test()%>%filter(PEOPLE_POSITIVE_NEW_CASES_COUNT==max(PEOPLE_POSITIVE_NEW_CASES_COUNT))
   )
  
  
  test2<-reactive(
    ggplot(test(),aes(x=REPORT_DATE,y=PEOPLE_POSITIVE_NEW_CASES_COUNT))+
    geom_line()+
      
      geom_point(aes(x=maximo()$REPORT_DATE,y=maximo()$PEOPLE_POSITIVE_NEW_CASES_COUNT),color="red")+
      
      geom_text(aes(x=maximo()$REPORT_DATE,y=maximo()$PEOPLE_POSITIVE_NEW_CASES_COUNT),
                label=paste0(maximo()$REPORT_DATE),color="red", vjust=-.5)+
      labs(x="Fecha", y="Casos de infectados diarios")
    
    )
  maximo2<-reactive(
    test()%>%filter(PEOPLE_DEATH_NEW_COUNT==max(PEOPLE_DEATH_NEW_COUNT))
  )
  
  
  test21<-reactive(
    ggplot(test(),aes(x=REPORT_DATE,y=PEOPLE_DEATH_NEW_COUNT))+
      geom_line()+
      
      geom_point(aes(x=maximo2()$REPORT_DATE,y=maximo2()$PEOPLE_DEATH_NEW_COUNT),color="red")+
      
      geom_text(aes(x=maximo2()$REPORT_DATE,y=maximo2()$PEOPLE_DEATH_NEW_COUNT),
                label=paste0(maximo2()$REPORT_DATE),color="red", vjust=-.5)+
      labs(x="Fecha", y="Muertes diarias")
    
  )
  
  ##### #####  #####  #####  #####  #####  #####  #####  #####  #####  #####  #####   
  
  
  
  datos_UY_Por_Fecha <- reactive(
    Datos %>% filter(COUNTRY == "Uruguay") %>% 
    select(REPORT_DATE, starts_with("People_")) %>% 
    arrange(REPORT_DATE)
    )
  
  fig1<-reactive(
    datos_UY_Por_Fecha()%>% ggplot() + 
    geom_line(aes(x=REPORT_DATE, y=PEOPLE_POSITIVE_NEW_CASES_COUNT, color = "Casos nuevos")) +
    geom_line(aes(x=REPORT_DATE, y=PEOPLE_DEATH_NEW_COUNT, color = "Fallecidos diariamente")) +
    geom_vline(aes(xintercept= ymd("2021-02-27"),color="Comienzo de vacunación"), 
               linetype="dashed", size= 1)+
    geom_text(aes(x=ymd("2021-02-27"),y=0),label=paste0("2021-02-27"),color="#d95f02",vjust=1.5,hjust=1.1)+
    labs(x= "Fecha", y = "Cantidad de personas") + 
    theme(axis.text.x = element_text(angle = 45, vjust=0.5, size = 8),
          legend.position = "right") + 
    scale_colour_brewer(palette = "Dark2") +
    guides(color=guide_legend(""))
    )
  

  

  
  
  
  
  
  
  #Calls
  output$mapa<-renderGirafe(girafe(ggobj = graf(),width_svg = 7, height_svg = 4, 
                                   options=list(opts_sizing(rescale=TRUE), 
                                                opts_hover(css = "fill:#C7B6DC;stroke:#C7B6DC;cursor:pointer;"))))
  
  
  
  output$tabla <- DT::renderDataTable({casos()})
  
  output$vacunas<- renderDygraph({dyg()})
  output$test33<- renderPlot({test2()})
  output$test32<- renderPlot({test21()})
  output$vacunacion2<-renderPlot({fig1()})
}





shinyApp(ui, server)