## app.R ##
library(shinydashboard)
library(ggmap)
library(dplyr)
library(leaflet)
library(data.table)

# Read in the data
flight.data <- read.csv("data.csv")
flight.data <- flight.data %>%
  select(ORIGIN,DEST,CARRIER,DAY_OF_MONTH,DEP_TIME,DEP_DELAY,ARR_DELAY,ARR_TIME)
airport.data <- fread("https://raw.githubusercontent.com/jpatokal/openflights/master/data/airports.dat")

# Subsetting data table
#order.by.origin <- as.data.frame(table(flight.data$ORIGIN))
#order.by.origin <- order.by.origin[order(-order.by.origin$Freq),c(1,2)]
#order.by.dest <- as.data.frame(table(flight.data$DEST))
#order.by.dest <- order.by.dest[order(-order.by.dest$Freq),c(1,2)]

# Cross reference the flites
# Origin airportID, lat, lng
origin.Data <- airport.data %>%
  select(V3,V5,V7,V8) %>%
  filter(V5 %in% flight.data$ORIGIN) %>%
  rename(ORIGIN = V5, origin.lat = V7, origin.lng = V8, city = V3)

# Destination airportTD, lat, lng
dest.Data <- airport.data %>%
  select(V5,V7,V8) %>%
  filter(V5 %in% flight.data$DEST) %>%
  rename(DEST = V5,dest.lat = V7, dest.lng = V8)

# Joing df to carry the necessary data for Visualization
flight.data <- merge(flight.data,origin.Data, by = "ORIGIN")
flight.data <- merge(flight.data,dest.Data, by ="DEST")

#total.airport.info <- unique(rbind(dest.Data,origin.Data))



ui <- dashboardPage(
  dashboardHeader(title = "Florida Airtraffic"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Widgets", icon = icon("th"), tabName = "widgets",
               badgeLabel = "new", badgeColor = "green")
    )
  ),
  ## Body content
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "dashboard",
              
              fluidPage(
                br(),
                column(8,leafletOutput("map", height="600px")),
                column(4,br(),br(),br(),br(),plotOutput("plot", height="300px")),
                br(),
                h2("Select starting City"),
                selectInput("city", "City", flight.data$ORIGIN,selected = "" )
                
              )
          
      ),
      
      # Second tab content
      tabItem(tabName = "widgets",
              h2("Widgets tab content")
      )
    )

  ))


#####



######

server <- function(input, output) {
  
  # build data with 2 places
  data=data.frame(x=c(130, 128), y=c(-22,-26), id=c("place1", "place2"))
  
  # create a reactive value that will store the click position
  data_of_click <- reactiveValues(clickedMarker=NULL)
  
  # Leaflet map with 2 markers
  output$map <- renderLeaflet({
    leaflet(flight.data )%>%
      addTiles() %>%
      setView(lat =28.4312, lng = -81.3081, zoom = 5) %>%
      addCircles(~dest.Data$dest.lng,~dest.Data$dest.lat ,popup = dest.Data$DEST, weight = 3, radius=40, 
                 color="red")
  })
  
  
  # store the click
  observeEvent(input$map_marker_click,{
    data_of_click$clickedMarker <- input$map_marker_click
  }) 
  
  # Make a barplot or scatterplot depending of the selected point
  output$plot=renderPlot({
    my_place=data_of_click$clickedMarker$id
    if(is.null(my_place)){my_place="place1"}
    if(my_place=="place1"){
      plot(rnorm(1000), col=rgb(0.9,0.4,0.1,0.3), cex=3, pch=20)
    }else{
      barplot(rnorm(10), col=rgb(0.1,0.4,0.9,0.3))
    }    
  })
}


shinyApp(ui, server)