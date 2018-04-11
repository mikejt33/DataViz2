# Make sure file path is set appropriately.  S
# Should be 
# Load the appropriate libraries
library(shinydashboard)
library(leaflet)
library(dplyr)
library(data.table)

# Read in the data
flight.data <- read.csv("Data/flights.csv")
airport.data <- fread("https://raw.githubusercontent.com/jpatokal/openflights/master/data/airports.dat")

# Subsetting data table
order.by.origin <- as.data.frame(table(flight.data$ORIGIN))
order.by.origin <- order.by.origin[order(-order.by.origin$Freq),c(1,2)]
order.by.dest <- as.data.frame(table(flight.data$DEST))
order.by.dest <- order.by.dest[order(-order.by.dest$Freq),c(1,2)]



# Cross reference the flites

# Setting map for leaflet (MCO - Orlando)
m <- leaflet() %>%
  setView(lat =28.4312, lng = -81.3081, zoom = 5) %>%
  addTiles()

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  output$distPlot <- renderPlot({
    
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2] 
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
    
  })
  
})
