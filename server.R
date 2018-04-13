# Make sure file path is set appropriately.  S
# Should be 
# Load the appropriate libraries
library(shinydashboard)
library(ggmap)
library(dplyr)
library(data.table)

# Read in the data
flight.data <- read.csv("Data/flights.csv")
flight.data <- flight.data %>%
  select(ORIGIN,DEST,CARRIER,DAY_OF_WEEK,FL_DATE,DEP_TIME,DEP_DELAY,ARR_DELAY,AIR_TIME,DISTANCE)
airport.data <- fread("https://raw.githubusercontent.com/jpatokal/openflights/master/data/airports.dat")

# Subsetting data table
order.by.origin <- as.data.frame(table(flight.data$ORIGIN))
order.by.origin <- order.by.origin[order(-order.by.origin$Freq),c(1,2)]
order.by.dest <- as.data.frame(table(flight.data$DEST))
order.by.dest <- order.by.dest[order(-order.by.dest$Freq),c(1,2)]

# Cross reference the flites
# Origin airportID, lat, lng
origin.Data <- airport.data %>%
  select(V5,V7,V8) %>%
  filter(V5 %in% flight.data$ORIGIN) %>%
  rename(ORIGIN = V5, origin.lat = V7, origin.lng = V8)

# Destination airportTD, lat, lng
dest.Data <- airport.data %>%
  select(V5,V7,V8) %>%
  filter(V5 %in% flight.data$DEST) %>%
  rename(DEST = V5,dest.lat = V7, dest.lng = V8)

# Joing df to carry the necessary data for Visualization
flight.data <- merge(flight.data,origin.Data, by = "ORIGIN")
flight.data <- merge(flight.data,dest.Data, by ="DEST")

total.airport.info <- unique(rbind(dest.Data,origin.Data))




florida.flight <- cbind()
  
total <- cbind(flight.data[match(names(total.airport.info$V7),flight.data$ORIGIN),],total.airport.info)

1
usa_center = as.numeric(geocode("United States")
                        
                        USAMap = ggmap(get_googlemap(center=usa_center, scale=2, zoom=4), extent="normal")
                        

# Total flight originating from Florida
florida.origin <- flight.data %>%
  select(ORIGIN,DEST)

total <- flight.data %>% inner_join()

# Setting map for leaflet (MCO - Orlando)
m <- leaflet() %>%
  setView(lat =28.4312, lng = -81.3081, zoom = 5) %>%
  addTiles() %>%
  addCircles(~V8,~V7, popup = dest.Data$DEST, weight = 3, radius=40, 
             color="#ffa500", stroke = TRUE, fillOpacity = 0.8)

m <- leaflet(dest.Data )%>%
  addTiles() %>%
  setView(lat =28.4312, lng = -81.3081, zoom = 5) %>%
  addCircles(~dest.Data$dest.lng,~dest.Data$dest.lat ,popup = dest.Data$DEST, weight = 3, radius=40, 
             color="red")


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  
  output$flightmap <- 
  
  m <- leaflet(dest.Data )%>%
    addTiles() %>%
    setView(lat =28.4312, lng = -81.3081, zoom = 5) %>%
    addCircles(~dest.Data$dest.lng,~dest.Data$dest.lat ,popup = dest.Data$DEST, weight = 3, radius=40, 
               color="red")
  
  map
  
})
