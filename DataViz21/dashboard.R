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


# Graphing Section

Florida_pops <- read.table("Florida_cities.csv", header = TRUE, sep = ",")
colnames(Florida_pops) <- c("city", "pop")
#Florida_pops$city <- tolower(Florida_pops$city)
#flight.data$city <- tolower(flight.data$city)

new <- as.data.frame(fl.airports$name)
colnames(new) <- "city"

for (i in new$city ) {
  new[new$city == i,"avg_delay"] <- mean(flight.data[flight.data$city == i,"DEP_DELAY"], na.rm = TRUE)
  new[new$city == i,"delayed_freq"] <- nrow(flight.data[flight.data$city == i & flight.data$DEP_DELAY > 15 ,])/nrow(flight.data[flight.data$city == i,])
  new[new$city == i,"total"] <- nrow(flight.data[flight.data$city == i,])
}

test <- merge(new, Florida_pops, by = "city")
test$pop <- as.numeric(gsub(",", "", test$pop))


ui <- dashboardPage(
  dashboardHeader(title = "Florida Airtraffic"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("plane")),
      menuItem("Data", icon = icon("table"), tabName = "stats"),
      menuItem("Delay Stats", icon = icon("wrench"), tabName = "delay"),
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
      ),
      tabItem(tabName = "delay",
              fluidPage(
                plotOutput('distPlot'),
                plotOutput("histPlot")
                
              ))
    )
  ))

server <- function(input, output) {
  # Leaflet map output
  output$map <- renderLeaflet({
    #Adding unique ID column
    flight.data$id <- seq.int(nrow(flight.data))
      
    a <- subset(flight.data, flight.data$city ==input$florigin & flight.data$DAY_OF_MONTH %in% (input$range[1]:input$range[2]))
    
    m <-leaflet() %>% setView(lat =28.4312, lng = -81.3081, zoom = 5) %>%
        addTiles() 
      for (i in 1:nrow(a))
        m <- m %>% addPolylines(data = a, lat = c(a[i,]$origin.lat, a[i,]$dest.lat),
                                lng = c(a[i,]$origin.lng, a[i,]$dest.lng),
                                weight = 2, opacity = .1, color = "green")
      m
      # TODO : Show avg delaytime per flight
  })
  

  ## Output for Data Table tab
  output$table <- renderDataTable(
   a <- subset(flight.data, flight.data$city ==input$florigin & flight.data$DAY_OF_MONTH %in% (input$range[1]:input$range[2]))
    
  )
  
  output$distPlot <- renderPlot({
    #a <- subset(flight.data, flight.data$city ==input$florigin & flight.data$DAY_OF_MONTH %in% range(input$range))
    ggplot(data = test,aes(x = test$pop, y =test$avg_delay )) + 
      geom_bar(stat = "identity",color = "black") + 
      labs(x = "Size of city",y = "avg delay time (in minutes)") + 
      theme_minimal()+ geom_bar(stat = "identity", data=test[test$city == input$florigin,], aes(x=pop, y=avg_delay), colour="green", size = 2)
  })
  
  output$histPlot <- renderPlot({
    ggplot(data = test,aes(x = test$delayed_freq)) + 
      geom_histogram(binwidth = .025, color = "black")+ 
      labs(x = "proportion of delayed flights", y = "count") +
      geom_histogram(data=test[test$city ==input$florigin ,], aes(x= delayed_freq),binwidth = .025, fill="lightblue")
  })
  
}

shinyApp(ui, server)