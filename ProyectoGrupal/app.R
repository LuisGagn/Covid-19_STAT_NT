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
library(ggiraph)

# DataSets || En caso de no descargar COVID.csv, comentar la fila 14 y descomentar la 13
#cgd<-read.csv("https://query.data.world/s/25w3mrdsnje6zupnls5pajt3ruf6in", header=TRUE, stringsAsFactors=FALSE);
cgd<- read.csv("COVID.csv")
vacunacion<- read.csv("https://raw.githubusercontent.com/3dgiordano/covid-19-uy-vacc-data/main/data/Uruguay.csv")
world <- ne_countries(scale = "medium", returnclass = "sf")

#UI SHINY
ui <- fluidPage(
  titlePanel("COVID-19 | Analisis"),
  tabsetPanel(
    tabPanel("Global",h1("Casos Globales"),
             sidebarLayout(
               sidebarPanel(
                 
                 sliderInput("fecha", "Selecciona la Fecha", min=as.Date("2020-01-21", "%Y-%m-%d"),
                             max=as.Date("2021-07-01", "%Y-%m-%d"), value=as.Date("2020-02-20"), ticks = FALSE),
                
                 selectInput("tipo", "Selecciona estilo", c("Diario","Total")),
                 selectInput("tipo2", "Selecciona Datos", c("Muertes","Infectados"))
                            ),mainPanel(p("Pasa el mouse sobre un pais para saber la cantidad de casos"),
                              girafeOutput("mapa"),DT::dataTableOutput("tabla")
               ))),    
    
    tabPanel("Vacunacion",h1("Test"),sidebarLayout(
               sidebarPanel(),mainPanel(
                 girafeOutput("vacunas", height = 700, width = 700)

               )))
))













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
   
   #################
   

  
    
  
    
 
    
    
    
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
    
    
    
# Vacunacion    
    vacunacion$date<- as.Date(vacunacion$date)
    vac2<-reactive( vacunacion%>%select(date,total_vaccinations, people_fully_vaccinated))
    
   
   a<- reactive(ggplot(vac2())+
  
#GEOM
     
     geom_line(aes(x=date,y=total_vaccinations),size=2,colour="#106D08")+
     geom_point_interactive(aes(x=date,y=total_vaccinations,tooltip=date, data_id=date, fill="a"),colour="#106D08")+
   
     
     geom_line(aes(x=date,y=people_fully_vaccinated),size=2,colour="#DC7633")+
     geom_point_interactive(aes(x=date,y=people_fully_vaccinated,tooltip=date, data_id=date, fill="b"),colour="#DC7633")+
    

    #TEMA
     scale_y_continuous(labels=comma)+scale_x_date(date_breaks = "1 month")+
     labs(y="Vacunas Totales", x="Fecha")+
     scale_fill_manual(name="Estado",
                       labels=c("a"="Vacunas Totales","b"="Personas vacunadas totalmente"),
                       values = c('#106D08','#DC7633'),
                       aesthetics = c("colour","fill"))+  
     guides(fill = guide_legend(override.aes = list(shape = 21)))
)
   
   
   
   
   
   
   
   
   
   
   
   
   
   
#Calls
  output$mapa<-renderGirafe(girafe(ggobj = graf(),width_svg = 7, height_svg = 4, 
                            options=list(opts_sizing(rescale=TRUE), 
                            opts_hover(css = "fill:#C7B6DC;stroke:#C7B6DC;cursor:pointer;"))))
  
  
  
  output$tabla <- DT::renderDataTable({casos()})

  
  
  output$vacunas <-  renderGirafe(girafe(ggobj = a(),width_svg = 7, height_svg = 4, 
                                  options=list(opts_sizing(rescale=TRUE),
                                               opts_zoom(min = 0.5, max = 1.5)
                                  )))
}





shinyApp(ui, server)