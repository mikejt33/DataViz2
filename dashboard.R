## app.R ##
library(shinydashboard)
library(ggmap)
library(dplyr) 
library(leaflet)
library(data.table)
library(geosphere)
library(bazar)
library(tidyr)
library(htmltools)
library(igraph)
library(networkD3)
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

flight.data$route <- paste(flight.data$ORIGIN,flight.data$DEST,sep = "-")
flight.data<- flight.data %>% drop_na()

b <- c(-Inf,0,5,10,15,20,25,Inf)
names <- c("No delay","Less than 5 min","5-10 min","10-15 min","15-20 min","20-25 min"," 25+ min")

flight.data$bin <- cut(flight.data$DEP_DELAY, breaks = b, labels = names)

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
      menuItem("Map", tabName = "dashboard", icon = icon("plane")),
      menuItem("Data", icon = icon("table"), tabName = "stats"),
      menuItem("City Stats", icon = icon("gg"), tabName = "network"),
      menuItem("Delay Stats", icon = icon("wrench"), tabName = "delay"),
      menuItem("Inputs", icon = icon("bar-chart-o"),
 
            
               conditionalPanel(
                 condition = "input.src == 'Dest'",
                 selectInput("Select Flight Destination", "Select Flight Destination",
                             choices = unique(flight.data$DEST), multiple=TRUE, selectize=TRUE,
                             width = '98%')
               ),
               selectInput("florigin", "Select Flight Origin",
                           choices = unique(fl.airports$name), multiple=FALSE, selectize=TRUE,
                           width = '98%'),
               sliderInput("range", "Day in Month Range:",
                           min = 1, max = 31,
                           value = c(1:31)),
               
               radioButtons("sort", "Would you like to sort by Delay Time",
                            c("No","Yes")),
               conditionalPanel(
                 condition = "input.sort == 'Yes'",
                 selectInput("dtime", "Select Delay Time",
                             choices = names, multiple=FALSE, selectize=TRUE,
                             width = '98%')
               )
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

    #input$florigin
    # (input$range[1]:input$range[2])
    
    if(input$sort == "Yes"){
      a <- subset(flight.data, flight.data$city == input$florigin & flight.data$bin == input$dtime & flight.data$DAY_OF_MONTH %in% (input$range[1]:input$range[2]))
    }
    else{
      a <- subset(flight.data, flight.data$city == input$florigin & flight.data$DAY_OF_MONTH %in% (input$range[1]:input$range[2]))
    }
    
    b <- a %>%
      group_by(route) %>%
      summarise(avg.Delay = mean(DEP_DELAY))
    
    
    total <- merge(a,b, by ="route")
    label <- total %>% 
      select(route,dest.lat, dest.lng, avg.Delay) %>%
      unique()
    
    label <- label %>% 
      mutate(avg.Delay = round(avg.Delay,2))
    
    sum1 <- total %>%
      group_by(route) %>%
      summarise(cnt = n())
    label <- merge(label,sum1, by = "route")
    
    label$avg.Delay <- as.character(label$avg.Delay)
    
    
    m <-leaflet() %>% setView(lat =28.4312, lng = -81.3081, zoom = 5) %>%
        addTiles() 
      for (i in 1:nrow(total))
        m <- m %>% addPolylines(data = total, lat = c(total[i,]$origin.lat, total[i,]$dest.lat),
                                lng = c(total[i,]$origin.lng, total[i,]$dest.lng),
                                weight = 2, opacity = .1, color = "black", popup =total[i,]$avg.Delay)
    
    m <- m %>% addTiles() %>%
      addMarkers(data = label, lat = ~dest.lat, lng = ~dest.lng, 
                 popup = paste(
                              "Route:", label$route, "<br>",
                               "Avg delay:", label$avg.Delay, " minutes", "<br>",
                               "Number of Flights: ", label$cnt))
    m
      # TODO : Show avg delaytime per flight
  }) 
  

  ## Output for Data Table tab
  output$table <- renderDataTable(
    
    if(input$sort == "Yes"){
      a <- subset(flight.data, flight.data$city == input$florigin & flight.data$bin == input$dtime & flight.data$DAY_OF_MONTH %in% (input$range[1]:input$range[2]))
    }
    else{
      a <- subset(flight.data, flight.data$city == input$florigin & flight.data$DAY_OF_MONTH %in% (input$range[1]:input$range[2]))
    }
 
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


 