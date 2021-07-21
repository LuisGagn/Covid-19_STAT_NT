# LIBRERIAS
# Shiny cosas
library(shiny)
library(shinydashboard)

# Datos mundiales (Para mapamundi)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(rgeos)

# Manejo Data
library(lubridate)
library(DT)
library(tidyverse)
library(readr)

# Graficos
library(scales)
library(ggplot2)
library(ggiraph)
library(dygraphs)
library(xts)
library(plotly)   



Datos<- read_csv("../Data/COVID-19 Activity.zip")
world <- ne_countries(scale = "medium", returnclass = "sf")%>%select(admin, sovereignt)
poblacion<- read_csv("../Data/poblacion.csv")


      ##########
      ## ui.R ##
      ##########

    #### Encabezado ####

encabezado <- dashboardHeader(title = "COVID-19 | ANÁLISIS")

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
                        c("Uruguay","Argentina","Brazil","Chile"))),
    
    # TASAS
    menuItem("Tasas", tabName="tasa", icon=icon("percent"),
             menuSubItem("Ver",tabName = "tasita",icon=icon("plus-square")),
             selectInput("tsa", "Seleccione el Pais",c("Uruguay", "Argentina", "Brazil", "Paraguay", "Chile", "Venezuela", "Peru", "Colombia", "Ecuador", "Todos")))
    
  ) # CLOSES SIDEBARMENU
  ) # CLOSES DASHBOARDSIDEBAR


#### Contenido ####
contenido <- dashboardBody(
  
 
  tabItems(
    tabItem(tabName = "inicio", h1("Nuevas tecnologías para el análisis estadístico de datos"),
            HTML('<center><img src="udelar.png", width=200,heigth=400></center>'),
            h3("Introducción | Coronavirus"),p("Coronavirus es una familia de virus conocidos por causar enfermedades que afectan las vías respiratorias de las personas, en mayor manera a la población de más edad y/o con enfermedades respiratorias o pobres condiciones físicas."),
                                 p("EL COVID-19 es la enfermadad causada por el coronavirus y nos ha afectado de gran manera en estos años. La misma se originó a fines del 2019 con una infección en China y fue declarada pandemia por la OMS el 30 de Enero de 2020, transformándose en una de las crisis sanitarias que más ha afectado a la humanidad."),
            HTML("<br><br>"),
            h4("Objetivo de la aplicación"), p("El objetivo principal de la aplicación es generar un informe interactivo que permita visualizar el efecto del COVID-19 en la población mundial, haciendo énfasis en América y la región. 
                                               A su vez, presentar visualizaciones que permitan interpretar cómo ha sido el proceso de vacunación en Uruguay y su impacto en la epidemia"),
                                             p("Para lograr que la app sea eficiente en términos de recursos, utilizamos programación reactiva, generando outputs que dependen de los inputs. Además de esto, se programaron diagramas y gráficos dinámicos, donde el usuario podrá interactuar e interpretar los resultados rápidamente, observando por su propia cuenta
                                                como ha sido la evolución del virus"),
            
            HTML("<br>"),
            h4("Informe"),p("Contamos adicionalmente con un informe donde se detalla información relevante que servirá de ayuda al momento de visualizar la aplicación e interpretar los gráficos y datos.",
                            a(href="https://raw.githubusercontent.com/LuisGagn/Covid-19_STAT_NT/main/RMD/COVID-19.pdf?token=ATTCHVE3EUMFQXYWTGGBMP3A46MTS","Link del PDF")),
            HTML("<br>"),
            h4("Desarrollo"), p("La aplicación se desarrolló utilizando el lenguaje de ciencia de datos: R, junto con la aplicacion RStudio, 
                                para poder ver el desarrollo de la misma, podrán ingresar al link del repositorio en GitHub enlistado en el final de esta diapositiva" ),
            HTML("<br>"),
            h4("Datos Utilizados"),p("Los datos utilizados fueron obtenidos de la plataforma", a(href="https://data.world/covid-19-data-resource-hub/covid-19-case-counts/workspace/file?filename=COVID-19+Activity.csv","data.world"), 
            "donde se actualizan a diario con información de todo el mundo, 
                                     también datos obtenidos del repositorio de:",a(href="https://github.com/3dgiordano/covid-19-uy-vacc-data","3dgiordano"),
            "los cuáles contienen información sobre la vacunación en Uruguay y son actualizados diariamente con datos del",a(href="https://monitor.uruguaysevacuna.gub.uy/","monitor de vacunacion")),
            
            
            
            
            HTML("<br><br><br>"),
            p("Proyecto STAT_NT | FCEA"),
            h4("Profesores:"),
            p("Natalia DaSilva | Federico Molina"),
            h4("Alumnos"),
            p("Nicolás Ferreira | Luis Gagñevin"),
            HTML("<br><br><br>"),
            p(icon=icon("github"), a(href="https://github.com/LuisGagn/Covid-19_STAT_NT", "Repositorio"))),
    
    
    
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
                         title = "Pico de Casos en el pais",status="warning",solidHeader = TRUE,width=350,
                         plotlyOutput("test33")
                     ),
                     box(title = "Pico de Muertes en el pais",status="danger",solidHeader = TRUE,width=350,
                         plotlyOutput("test32")
                       
                     )
                     )
              ),
              tabPanel("Vacunación",
                       fluidRow(
                         box(
                           title = "Efectos de la Vacunacion",status="success",solidHeader = TRUE,width=350,
                           plotlyOutput("vacunacion2")
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
          )
          )
),# CLOSES TABITEM MAPA

tabItem(tabName="tasita",
        tabsetPanel(
          tabPanel("Tasa por pais",
                   fluidRow(
                     box(
                       title="Tasa de Mortalidad e Infeccion", status="danger",solidHeader = TRUE,
                       plotlyOutput("tasasmi")
                     ),
                     
                       box(
                        title="  Tabla de datos", status="warning", solidHeader = TRUE,
                        tableOutput("tabpais")
                       )
                    
                   ))
        ))



) # CLOSES TAB ITEMS
) # CLOSES DASHBOARD BODY





## UI - Interfaz del Usuario ##

ui <- dashboardPage(skin = "purple", encabezado, barra_lateral, contenido)





###########################






server <- function(input, output, session) {
  
  


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
      summarise("Casos"=sum(PEOPLE_POSITIVE_CASES_COUNT)/1000000)  
    
  }else if(input$tipo=="Diario" && input$tipo2=="Infectados"){
    casos2() %>% 
      summarise("Casos"=sum(PEOPLE_POSITIVE_NEW_CASES_COUNT)/1000000) 
    
  }else if(input$tipo=="Total" && input$tipo2=="Muertes"){
    casos2() %>% 
      summarise("Casos"=sum(PEOPLE_DEATH_COUNT)/1000)
    
  }else{
    casos2() %>% 
      summarise("Casos"=sum(PEOPLE_DEATH_NEW_COUNT)/1000)
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
      graf2()+labs(fill="Muertes (Miles)")+
        scale_fill_gradientn(colours = c("#f7e6e6","#bf3e3e","#340404"),
                             na.value = "grey50",label=comma
        )
    }else{
      graf2()+labs(fill="Infectados (Millones)")+
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

  # PICOS DE CONTAGIO | MUERTES
  
  # PICO DE CONTAGIOS
  #  SELECCIONA FECHA DEL PICO
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
  
  #  PICO DE MUERTES
  #  SELECCIONA FECHA DEL PICO
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
  
  # GRAFICO DE URUGUAY SOBRE EVOLUCION GENERAL DEL COVID
  
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
  
  ##### #####  #####  #####  #####  #####  #####  #####  #####  #####  #####  #####   
  
  # TASAS DE MUERTES | TASAS DE CONTAGIOS
  
  # SELECCIONA PAISES
  Datos_Region2<- reactive(
    Datos %>% 
      group_by(COUNTRY)%>%
      filter(COUNTRY==c("Uruguay", "Argentina", "Brazil", "Paraguay", "Chile", "Venezuela", "Peru", "Colombia", "Ecuador"))%>%
      select(REPORT_DATE, starts_with("People_"))%>%
      arrange(REPORT_DATE)
  )
  
  Datos_Region <- reactive(
    if(input$tsa=="Todos"){
    Datos_Region2()
    }else{
      Datos_Region2() %>% 
        filter(COUNTRY == input$tsa)
}
      )
  # POBLACION SUMADA A LA DATA 
  popregion <- reactive(
    poblacion %>% 
    merge(Datos_Region(), by.x="Location", by.y="COUNTRY")%>%
    group_by(Location)%>%
    summarise(infeccion=round((PEOPLE_POSITIVE_CASES_COUNT/(PopTotal*1000)),digits=2), REPORT_DATE,letal=round((PEOPLE_DEATH_COUNT/PEOPLE_POSITIVE_CASES_COUNT),digits=2))
  
  )
  
  # GRAFICO 
  tasagraf<- reactive(
    popregion() %>% ggplot() + 
      geom_line(aes(x=REPORT_DATE, y=infeccion, color = "Tasa de infección")) +
      geom_line(aes(x=REPORT_DATE, y=letal, color = "Tasa de letalidad")) + 
      facet_wrap(~Location) + 
      labs(x = "Fecha", y = "Porcentaje afectado") +
      theme(axis.text.x = element_text(angle = 45, vjust=0.5, size = 6),
            legend.position = "bottom", legend.title = element_blank()) + scale_colour_brewer(palette = "Dark2")+
      scale_y_continuous(labels=scales::percent_format(accuracy = 5)) 
  )
  
  # TABLA
  cuadro2 <- reactive(popregion() %>% 
    filter(letal>0) %>% 
    group_by(Location) %>% 
    summarise("Promedio de Infección" = mean(infeccion)*100,
              "Promedio de Letalidad" = mean(letal)*100,
              "Máximo de infección" = as.character(REPORT_DATE [which.max(infeccion)]),
              "Máximo de letalidad" = as.character(REPORT_DATE [which.max(letal)]))
  )

  
  
  
  
  
  
  #Calls
  output$mapa<-renderGirafe(girafe(ggobj = graf(),width_svg = 10, height_svg = 3, 
                                   options=list(opts_sizing(rescale=TRUE), 
                                                opts_hover(css = "fill:#C7B6DC;stroke:#C7B6DC;cursor:pointer;"))))
  
  
  
 
  output$vacunas<- renderDygraph({dyg()})
  output$test33<- renderPlotly({test2()})
  output$test32<- renderPlotly({test21()})
  output$vacunacion2<-renderPlotly({fig1()})
  output$tasasmi<- renderPlotly({tasagraf()})
  output$tabpais<- renderTable({cuadro2()})
}





shinyApp(ui, server)
