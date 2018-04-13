## app.R ##
library(shinydashboard)
library(ggmap)
library(dplyr)
library(leaflet)
library(data.table)

# Read in the data
# Add city name
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
      menuItem("Stats", icon = icon("bar-chart-o"), tabName = "stats"),
      menuItem("Inputs", icon = icon("bar-chart-o"),
               
               radioButtons("src", "Choose Origin or Destination:",
                            c("Flights originating from" = "org",
                              "Flights whose destination is" = "Dest")),
               conditionalPanel(
                 condition = "input.src == 'org'",
                 selectInput("Select Flight Origin", "Select Flight Origin",
                             choices = unique(flight.data$ORIGIN), multiple=TRUE, selectize=TRUE,
                             width = '98%')
               ),
               conditionalPanel(
                 condition = "input.src == 'Dest'",
                 selectInput("Select Flight Destination", "Select Flight Destination",
                             choices = unique(flight.data$DEST), multiple=TRUE, selectize=TRUE,
                             width = '98%')
               ),
               
               sliderInput("range", "Tme Range:",
                           min = 1, max = 31,
                           value = c(1:31)),
               
               radioButtons("sort", "Would you like to sort by Carrier",
                            c("Yes","No"))
               
               
               
               )
               # Input directly under menuItem

    )
  ),
  ## Body content
  dashboardBody(
    tags$body(tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}")),
    tabItems(
      # First tab content
      tabItem(tabName = "dashboard",
              
              fluidPage(
                leafletOutput("map", height="800px", width = "100%")
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
 
  
  # Make a barplot or scatterplot depending of the selected point

}


shinyApp(ui, server)