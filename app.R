library(shiny)
library(tidyverse)
library(leaflet)

usa <- read_rds("usa_location.RDS")
t.storm <- read_rds("nlihc-tweets.RDS")
t.storm <- t.storm %>% filter(!is.na(lat) | !is.na(lng))

ui <- fluidPage(
  
  navbarPage(title = "#OurHomesOurVotes2020 Tweetstorm: Sept. 11, 2019 3:00 - 4:00 EST" ,
  
             tabPanel("Map" , 
                      
  div(class="outer",
      
      tags$head(
       
        includeCSS("www/styles.css")
      ),
  
  leafletOutput("map" , width = "100%" , height = "100%") ,
  
  absolutePanel(id = "run-again", class = "panel panel-default", fixed = TRUE,
                draggable = TRUE, top = "auto", left = 60, right = "auto", bottom = 60,
                width = "auto", height = "auto", 
                
                actionButton("run.again" , "Run Again")
                
                ) 
  )
    
  
  ) )
  
      
)

server <- function(input, output) {
  
  start.time <- Sys.time()
  
  renderPopup <-  function(){
    
    diff.time <- as.integer(difftime(Sys.time() , start.time , units = "secs"))
    
    if (diff.time <= 60 ) { 
      
      popup.data <- t.storm %>% filter(minute == diff.time)
      
      popup.data
      
    } else {
      
      popup.data <- t.storm %>% filter(minute == diff.time)
      
      popup.data
      
      }

  }
  
  renderMap <-  function(){
    
    diff.time <- as.integer(difftime(Sys.time() , start.time , units = "secs"))
    
    if (diff.time <= 60 ) {
      
      map.data <- t.storm %>% filter(minute %in% c(0:diff.time)) %>% 
        group_by(lat  , lng) %>% 
        mutate(size = 2+ sqrt(dplyr::n()+5))
      
      map.data
      
    } else {
      
      map.data <- t.storm %>% 
        group_by(lat  , lng) %>% 
        mutate(size = 2+ sqrt(dplyr::n()+5))
      
      map.data
      
    }
    
    
    
  }
  
  values <- reactiveValues(df  = renderMap())
  
  
  output$map <- renderLeaflet({
 
    leaflet(values$df) %>% 
      addTiles(
        urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      ) %>% 
      fitBounds(lng1 = usa[["box"]][["sw.lng"]] ,
                   lng2 = usa[["box"]][["ne.lng"]] ,
                   lat1 = usa[["box"]][["sw.lat"]] , 
                   lat2 = usa[["box"]][["ne.lat"]]
      ) %>% 
      leaflet.extras::addFullscreenControl()
    
  })
  
  observe({
    
    invalidateLater(1000)
    
    leafletProxy("map") %>% 
      addCircleMarkers(data = renderMap() ,
                       radius = ~size ,
                       weight  = 1 ,
                       fillOpacity = .6 ,
                       color = "white" ,
                       fillColor = "#98CAEC" ,
                       popup = ~embedded
                      ) %>% 
      addPopups(data = renderPopup() , 
                popup = ~embedded , 
                group = "popups"
                ) %>% 
      addLayersControl(overlayGroups = "popups" , 
                       options = layersControlOptions(collapsed = F)
                      )
    
    })
  
  observe({
    
    invalidateLater(1700)
    
    leafletProxy("map") %>% 
      clearGroup("popups")
     
 })
  

  observeEvent(input$run.again , {
    
    start.time <<- Sys.time()
    
    leafletProxy("map") %>% 
      clearPopups() %>% 
      clearMarkers()
    
  })
  
   
}

# Run the application 
shinyApp(ui = ui, server = server)
