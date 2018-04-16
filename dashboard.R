## app.R ##
library(shinydashboard)
library(ggmap)
library(dplyr)
library(leaflet)
library(data.table)
library(geosphere)
library(bazar)
set.seed(678)

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

fl.airports <- c("PNS","VPS","ECP",
                 "TLH", "JAX","GNV",
                 "DAB","SFB","MCO",
                 "MLB","TPA","PIE",
                 "SRQ","PGD","RSW",
                 "PBI","FLL","MIA","EYW")
fl.airports <- data.frame(fl.airports)
fl.airports$name <- c("Pensacola","Destin-Fort Walton","Panama City",
                      "Tallahassee","Jacksonville","Gainesville",
                      "Daytona Beach","Orlando-Sanford","Orlando",
                      "Orlando-Melbourne","Tampa","St.Pete-Clearwater",
                      "Sarasota","Punta Gorda","Fort Myers",
                      "Palm Beach","For Lauderdale","Miami","Key West")
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

flight.data <- flight.data %>%
  filter(flight.data$ORIGIN %in% fl.airports$fl.airports)


#total.airport.info <- unique(rbind(dest.Data,origin.Data))



ui <- dashboardPage(
  dashboardHeader(title = "Florida Airtraffic"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Stats", icon = icon("bar-chart-o"), tabName = "stats"),
      menuItem("Inputs", icon = icon("bar-chart-o"),

            
               conditionalPanel(
                 condition = "input.src == 'Dest'",
                 selectInput("Select Flight Destination", "Select Flight Destination",
                             choices = unique(flight.data$DEST), multiple=TRUE, selectize=TRUE,
                             width = '98%')
               ),
               selectInput("florigin", "Select Flight Origin",
                           choices = unique(fl.airports$name), multiple=TRUE, selectize=TRUE,
                           width = '98%'),
               sliderInput("range", "Day in Month Range:",
                           min = 1, max = 31,
                           value = c(1:31)),
               
               radioButtons("sort", "Would you like to sort by Carrier",
                            c("Yes","No")),
               conditionalPanel(
                 condition = "input.sort == 'Yes'",
                 selectInput("Select Air Carrier", "Select Air Carrier",
                             choices = unique(flight.data$CARRIER), multiple=TRUE, selectize=TRUE,
                             width = '98%')
               ),
               
               actionButton("check",
                            "Check out some Stats!
                            ")
               
               
               
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
                renderPrint("text"),
                leafletOutput("map", height="800px", width = "100%")
              )
          
      ),
      
      # Second tab content
      tabItem(tabName = "stats",
              fluidPage(
                dataTableOutput('table')
              )
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
    

 
    
    #flight.data <- sample_n(flight.data,1000)
    flight.data$id <- seq.int(nrow(flight.data))
    
    a <- subset(flight.data, flight.data$city ==input$florigin & flight.data$DAY_OF_MONTH %in% (input$range[1]:input$range[2]))
    
    m <-leaflet() %>% setView(lat =28.4312, lng = -81.3081, zoom = 5) %>%
        addTiles() 
      for (i in 1:nrow(a))
        m <- m %>% addPolylines(lat = c(a[i,]$origin.lat, a[i,]$dest.lat),
                                lng = c(a[i,]$origin.lng, a[i,]$dest.lng),
                                weight = 2, opacity = .1, color = "green")
      m
      # TODO : Show avg delaytime per flight
  })
  


  output$table <- renderDataTable(
    
    

   a <- subset(flight.data, flight.data$city ==input$florigin & flight.data$DAY_OF_MONTH %in% (input$range[1]:input$range[2]))
    
  )
 

  
  
  # store the click
 
  
  # Make a barplot or scatterplot depending of the selected point

}


shinyApp(ui, server)