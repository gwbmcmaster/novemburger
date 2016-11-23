library(leaflet)
library(shiny)
library(dplyr)

burger <- read.csv("burgerFinal.csv", stringsAsFactors = FALSE)

iconL <- makeIcon(iconUrl = "http://megaicons.net/static/img/icons_title/409/1242/title/burger-2-icon.png")
burger$t <- as.character(gsub("\\$", "", burger$Price.x))
burger$t[13] <- 7 
burger$t <- as.numeric(burger$t)
burger <- burger[, c(2:8, 10:15,18,40,41,58)]

ui <- bootstrapPage(
  h1("Novemburger - Hamilton, ON", align = "center"),
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  includeCSS("style.css"),
  absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE, 
                draggable = TRUE, top = 70, right = 0, bottom = "auto",
                width = 350, height = "auto", 
                h2("Hamburger Explorer", align = "center"),
                sliderInput("priceInput", "Price", min = min(burger$t), max = max(burger$t),
                            value = c(5,10), step = 0.5, pre = "$"),
                selectInput("cityInput", "Location", choices = c("Hamilton", "Dundas", "Ancaster", "Waterdown", "Stoney Creek"),
                            selected = "Hamilton")
  )
  
)

server <- function(input, output){
             filteredData <- reactive({burger %>% filter(t >= input$priceInput[1], 
                        t <= input$priceInput[2],
                      City == input$cityInput)
    })
   output$map <- renderLeaflet({
     leaflet(burger) %>% addTiles() %>% setView(-79.8622, 43.244, zoom = 12)
   })
   observe({
     leafletProxy("map", data = filteredData()) %>% clearMarkers() %>% addMarkers(~newLon, ~newLat, icon = iconL, popup = ~paste("<strong>Restaurant:</strong><a href=", as.character(Website.x),">", as.character(Restaurant.x),"</a><br>",
                                                                                                                                 "<strong>Price:</strong>$",t,"<br>",
                                                                                                                                 "<strong>Phone:</strong>", Phone.x,"<br><br>",
                                                                                                                                 "<center><img src=", as.character(Photo.x),"width=150px; height=135px;/></center><br>",
                                                                                                                                 "<strong>Directions:</strong><a href=", paste(as.character('https://www.google.ca/maps/dir//'), 
                                                                                                                                                                               as.character(gsub(' ', '+', Address)), 
                                                                                                                                                                               as.character(',+'), as.character(City), ',+ON/>', sep=""), 
                                                                                                                                  as.character(Address), "</a>"))
   })
}

shinyApp(ui = ui, server = server)