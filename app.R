# Jonathan Conrow
# August 6, 2018
# Stockton University
# DSSA Data Practicum
#

#set the working directory
#setwd("")

#Import the proper library to look at the data and create the app
library(tidyverse)
library(jsonlite)
library(geojsonio)
library(shinydashboard)
library(shiny)
library(leaflet)
library(rsconnect)
library(ggplot2)



#Import the different data sets from http://insideairbnb.com/get-the-data.html
calendar <- read_csv("calendar.csv")
listings <- read_csv("listings.csv")
neighborhoods <- read_csv("neighbourhoods.csv")
reviews <- read_csv("reviews.csv")

#read in the geojson file
geojson <- geojson_read("neighbourhoods.geojson", what = "sp")
#geojson <- readLines("neighbourhoods.geojson")

#as the listings only gives us the number of available days in the year we need to make a variable populated with the number of days it was booked for the year
listings$booked <- 365-listings$availability_365

#Now it is important to use the assumption that the blocked outdates for the year count as the rental location is booked and not just "blocked out" that way we can get a general idea of how much money the property is generating each year.
listings$yearlyRevenue <- listings$price * listings$booked
summary(listings)

#Code for making histograms
n_breaks <- sqrt(nrow(listings))
breaksRevenue <- seq(from=min(listings$yearlyRevenue),
                     to=max(listings$yearlyRevenue))
revenueHist <- hist(listings$yearlyRevenue, breaks=n_breaks, main = "Yearly Revenue", xlab = "Yearly Revenue")

breaksPrice <- seq(from=min(listings$price),
                   to=max(listings$price))
priceHist <- hist(listings$price, breaks=breaksPrice, main = "Price Per Night", xlab = "Nightly Price")

#Outlier Cutoff, as the many outliers are causing the distribution to be long tail, I will use the general rule of thumb and cut out the outliers that are bigger than (Q3+1.5*IQR)
outlier_cutoff <- quantile(listings$yearlyRevenue, 0.75)+1.5*IQR(listings$yearlyRevenue)
index_outlier_ROT <- which(listings$yearlyRevenue > outlier_cutoff)
yearly_revenue_ROT <- listings[-index_outlier_ROT,]

#Still an issue
#As there is a frequency of rental properties that are listed for $0 a night, 
#we are not interested in the rental locations that are available for free, 
#it does not give us a good idea of what kind of evenue can be generated from 
#renting out your home. This would also indicate a large number of 
#unbooked properties for the year. As of now it is no interest to us.

#Lets make a descision to exclude the unbooked properties.
index_outlier_greaterThan0 <- which(yearly_revenue_ROT$yearlyRevenue < 1)
data_greaterThan0 <- yearly_revenue_ROT[-index_outlier_greaterThan0,]

#Indexing for entire home/apt rentals in mission bay only as the entire home rental and mission bay
#both generate the most estimated revenue
missionBay <- subset(data_greaterThan0, neighbourhood == "Mission Bay")
entireHomeMissionBay <- subset(missionBay, room_type == "Entire home/apt")

#indexing by neighborhood so it can be called in the app
neighborhoods <- as.character(geojson$neighbourhood)


#Make the Ui portion of the shiny app
ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "San Diego AirBnB, Where to Invest",
                  titleWidth = 450),
  dashboardSidebar(
    collapsed = TRUE,
    sidebarMenu(
      menuItem("Map", tabName = "mapss", icon = icon("map-pin")),
      menuItem("Stats", tabName = "stats", icon = icon("signal")),
      menuItem("Source Data", tabName = "sourceData", icon = icon("download")),
      menuItem("Code", tabName = "sourceCode", icon = icon("code"))
    )
  ),
  dashboardBody(
    tabItems(
      #Mapss tab Item
      tabItem(tabName = "mapss",
              fluidPage(
                #leflet map output in this dashboard
                box(leafletOutput("myMap")),
                
                #select inputbox for map
                box(
                  title = "Select Your Location and Rental Type",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  background = "black",
                  footer = TRUE,
                  checkboxGroupInput("selectRoom","Select a Rental Type",
                                     c("Entire Home/Apt" = "Entire home/apt",
                                       "Private Room" = "Private room",
                                       "Shared Room" = "Shared room")),
                  p(class = "text-muted",
                    br(),
                    "By clicking on a marker you can view the estimated yearly income of the property, derived from the yearly blocked out days and the cost per night "),
                  selectInput("selectNeighborhood", "View Neighborhoods", 
                              c(All = "all", neighborhoods))
                ),
                
                
                #Add a action button for clearing the tiles
                actionButton("clear", "Clear Map", icon = icon("refresh"))
              ),
              #Infoboxes providing data for the app
              fluidRow(box(
                column(width = 1,
                       fluidRow(
                         infoBox("Rental Locations", 9486, icon = icon("hashtag"))
                       ),
                       fluidRow(
                         infoBox("Mean Yearly Profit", round(mean(data_greaterThan0$yearlyRevenue),2),icon = icon("usd"))
                       )
                )
              )
              ),
              p(class = "text-muted",
                "This web application and all of its parts have no formal connection to any of the listed sites or AirBnB itself. It is strictly for educational purposes and a interest in analysis of the sharing econonmy data.")
      ),
      
      #Second Tab Content
      tabItem(tabName = "stats",
              fluidRow(
                column(
                  width = 12,
                  #show a plot of distribution
                  h3("Distribution of Estimated Yearly Revenue"),
                  plotOutput("distPlot"),
                  p(class = "text-muted",
                    "The above histogram was derived for trimming down the original data set and excluding any propertys that were listed for $0, as they are not generating revenue. The Estimated revenue value was derived from the number of blocked calender dates (assuming these are bookings and not just blocked days) and multiplying them by the Price per night. This method may not be very accurate but may provide insight into the type of information that may be translatable to other industries. "),
                  #show a barchart of the rental types and how much revenue they are generating,
                  br(),
                  h3("Bar Chart of Estimated Yearly Revenue per Rental Type"),
                  plotOutput("barChart"),
                  p(class = "text-muted",
                    "To further investigate the distribution, I thought it would be interesting to see what the estimated revenue would be depending on the rental type, as in the distribution it is clear that a entire home/ apartment rental is the most predominant rental type. As we can see from this chart, all of the home/apt rental accounts for over $200 million generated."),
                  br(),
                  h3("Bar Chart of Estimated Yearly Revenue by Neighborhood"),
                  plotOutput("barChart2"),
                  p(class = "text-muted",
                    "Looking Further into what locations generate the most revenue, we can see that Mission Bay, California generates an estimated $40 million in AirBnB rental revenue "),
                  br(),
                  h3("Smoothed Conditional of The Estimated Yearly Revenue as a Function of Nightly Cost for a Entire Home/Apt in Mission Bay"),
                  plotOutput("smoothedCond"),
                  p(class = "text-muted",
                    "We can now see a trend clearly in the Mission Bay subset of data that only contains Entire Home/ Apartment Rental listings. It appears as the nightly rate goes up so does the estimated yearly revenue. This could potentially indicate that there is a steady number of bookings in Mission Bay and/or there is a high demand for rentals in this location and not a large supply. This may interest housing developers looking to build homes strictly to rent for AirBnB Purposes.")
                )
              )
      ),
      
      #Third Tab Content
      tabItem(tabName = "sourceData",
              fluidPage(
                h2("Download The Source Data"),
                h4("Source data can be downloaded from the link below"),
                htmlOutput("hyperlink")
              )
      ),
      
      tabItem(tabName = "sourceCode",
              fluidPage(
                h2("Download The Source Code"),
                h4("The code for this entire app and project can be down loaded from the link below"),
                htmlOutput("codeHyperlink")
              )
      )
    )
  )
)

#make the server portion of the app
server <- function(input, output) {
  
  
  filterData <- eventReactive(input$selectRoom, {
    subset(data_greaterThan0, room_type == input$selectRoom)
  }, ignoreNULL = TRUE)
  
  #make a reactive value that could be called for a event reactive based on the geojson neighborhood polygons
  neighReactive <- reactive(geojson)
  
  #event reactive to be called for graphing the neighborhoods
  filterNeighborhood <- eventReactive(input$selectNeighborhood, {
    if(input$selectNeighborhood == "all"){
      neighReactive()
    }else{subset(geojson, neighbourhood == input$selectNeighborhood)
    }
  })
  #filterNeighborhood <- eventReactive(input$selectNeighborhood, {
  # subset(geojson, neighbourhood == input$selectNeighborhood)
  #}, ignoreNULL = TRUE)
  
  
  #Use leaflet here for the base map
  output$myMap <- renderLeaflet({
    leaflet(data_greaterThan0) %>%
      addProviderTiles(providers$Stamen.Toner) %>%
      fitBounds(~min(longitude), ~min(latitude), ~max(longitude), ~max(latitude))
    
  })
  
  #Makeing incramental changes to the map using proxy layers
  #Input for the room types
  observeEvent(input$selectRoom, {
    leafletProxy("myMap", data = filterData()) %>%
      clearMarkerClusters() %>%
      addMarkers(~longitude, ~latitude,
                 popup = ~paste0("Estimated Yearly Revenue $",yearlyRevenue),
                 #markerOptions(clickable = TRUE, riseOnHover = TRUE),
                 clusterOptions = markerClusterOptions(showCoverageOnHover = FALSE,
                                                       spiderfyOnMaxZoom = TRUE)) %>%
      fitBounds(~min(longitude), ~min(latitude), ~max(longitude), ~max(latitude))
  })
  
  #Input for the geojson
  observeEvent(input$selectNeighborhood, {
    leafletProxy("myMap", data = filterNeighborhood()) %>%
      clearShapes() %>%
      addPolygons(stroke = FALSE,
                  weight = 2,
                  opacity = 1,
                  color = "pink",
                  dashArray = "1",
                  smoothFactor = 0.3, 
                  fillOpacity = 0.4, 
                  fillColor = "blue", 
                  label = ~paste0(neighbourhood),
                  highlight = highlightOptions(
                    stroke = TRUE,
                    weight = 3,
                    color = "pink",
                    fillOpacity = 0.7,
                    bringToFront = TRUE
                  ))
  })
  
  #Create a button that clears all markers form the mapp
  observeEvent(input$clear, {
    leafletProxy("myMap", data = filterData()) %>%
      clearMarkerClusters()
  })
  
  output$codeHyperlink <- renderUI({
    tags$a(href="https://github.com/JonCon732/airbnb_analysis_web_app/blob/master/jonconAirbnbAnalysis.md", "Click here!")
  })
  
  output$hyperlink <- renderUI({
    tags$a(href="http://insideairbnb.com/get-the-data.html", "Click here!")
  })
  
  #Output a ggplot of the Histogram of estimated yearly revenue
  output$distPlot <- renderPlot({
    ggplot(data_greaterThan0, xName="Yearly Revenue", groupName="Room Type",   aes(yearlyRevenue, fill= room_type)) +
      geom_histogram(binwidth = 500) +
      ggtitle("Distibution of Estimated Yearly Revenue")+
      xlab("Estimated Yearly Revenue in USD ($)") +
      theme_bw()+
      scale_x_continuous(labels = scales::dollar)
  })
  
  output$barChart2 <- renderPlot({
    ggplot(data_greaterThan0, aes(neighbourhood, yearlyRevenue))+
      ylab("Estimated Yearly Revenue in USD ($)") + xlab("Neighborhood")+
      ggtitle("Estimated Yearly Revenue by Neighborhood")+
      geom_col(na.rm = TRUE)+
      theme_bw()+
      theme(axis.text.x = element_text(size = 9, angle = 90))+
      theme(axis.ticks = element_blank())+
      scale_y_continuous(labels = scales::dollar)
  })
  
  output$barChart <- renderPlot({
    ggplot(data_greaterThan0, aes(room_type, yearlyRevenue, fill = room_type), groupName = "Rental Type")+
      ylab("Estimated Yearly Revenue in USD ($)") + xlab("Room Type")+
      ggtitle("Estimated Yearly Revenue by Rental Type")+
      geom_col(na.rm = TRUE)+
      theme_bw()+
      theme(axis.text.x = element_text(size = 15))+
      theme(axis.ticks = element_blank())+
      scale_y_continuous(labels = scales::dollar)
  })
  
  output$smoothedCond <- renderPlot({
    ggplot(entireHomeMissionBay, aes(price, yearlyRevenue))+
      ylab("Estimated Yearly Revenue in USD ($)") + xlab("Price Per Night USD ($)")+
      ggtitle("Smoothed Conditional Yearly Revenue of Entire Home/ Apt Rentals in Mission Bay")+
      geom_smooth()+
      theme_bw()+
      theme(axis.ticks = element_blank())+
      scale_y_continuous(labels = scales::dollar)+
      scale_x_continuous(labels = scales::dollar)
  })
  #  output$histTable <- renderTable({
  #    summary(data_greaterThan0)
  #  })
  
}


shinyApp(ui, server)
