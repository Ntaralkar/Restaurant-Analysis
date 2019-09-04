library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(plotly)
library(sqldf)
library(tidyverse)
library(leaflet)
library(rgeos)
library(data.table)
library(magrittr)
library(ggplot2)
library(lubridate)
library(xts)
library(forecast)
library(DT)
library(gridExtra)
library(leaflet)
library(htmltools)
library(mapdata)
library(maptools)
library(sp)
library(googleAuthR)
library(ggmap)

setwd("C:/Users/nikhi/Desktop/MyAnalysis/R Shiny/")

air_reserve = read.csv("air_reserve.csv", header = T,stringsAsFactors = F)
air_store = read.csv("air_store_info.csv", header = T,stringsAsFactors = F)
air_visit = read.csv("air_visit_data.csv", header = T,stringsAsFactors = F)
hpg_reserve <- read.csv("hpg_reserve.csv", header = T,stringsAsFactors = F)
store_ids <- read.csv("store_id_relation.csv",header = T,stringsAsFactors = F)
air_visit$visit_date = as.Date(air_visit$visit_date)
air_visit$week = strftime(air_visit$visit_date, format = "%V")
air_visit$month = as.numeric(month(air_visit$visit_date))
air_visit$year = as.numeric(year(air_visit$visit_date))

final_data = fread("final_restaurant_data.csv")
final_data$week = strftime(final_data$visit_date, format = "%V")
final_data$month = as.numeric(month(final_data$visit_date))
final_data$year = as.numeric(year(final_data$visit_date))
final_data$week <- as.numeric(final_data$week)

#Area name changes
final_data[, location := tstrsplit(air_area_name, split = " ", keep = 1)]
final_data[, location := gsub('-ken', '', location)]
final_data[, location := gsub('-fu', '', location)]
final_data[, location := gsub('-to', '', location)]



register_google(key="AIzaSyDfRPKqjm06BmTOMLtsBmu2y3jbdJ-yMzo")



ui <- dashboardPage(
  dashboardHeader(title = "Recruit Holdings!"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction", tabName = "Introduction", icon = icon("dashboard")),
      #menuItem("BRD1", tabName = "BRD1", icon = icon("th")),
      menuItem("Map Overview", tabName = "BRD2", icon = icon("th")),
      menuItem("Month Analysis", tabName = "BRD3_1", icon = icon("th")),
      menuItem("Week Analysis", tabName = "BRD3_2", icon = icon("th")),
      menuItem("Location", tabName = "BRD4", icon = icon("th")),
      menuItem("Area Exploration", tabName = "BRD5", icon = icon("th")),
      menuItem("Genre Exploration", tabName = "BRD6", icon = icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "Introduction",
              fluidRow(
                box(h3("Restaurant Analysis"), br(), h4("This shiny application shows data analysis for Recruitment Holdings company. Recruit Holdings has unique access to key datasets that could make automated future customer prediction possible. Specifically, Recruit Holdings owns Hot Pepper Gourmet (a restaurant review service), AirREGI (a restaurant point of sales service), and Restaurant Board (reservation log management software).", br(), h4("User can view customers and stores in the application and navigate numbers in total level, at store level and region level", br(), 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                      h4("User can also view historical customers by stores for the last week/month in the Shiny app"))) 
              ,width = 12))),
      
      # Second tab content
      
      tabItem(tabName = "BRD1", plotlyOutput("brd1")),
      tabItem(tabName = "BRD2", leafletOutput("brd2"), h5("This is a fully interactive and zoomable map of all the air restaurants. Click on the clusters to break them up into smaller clusters and ultimately into the individual restaurants, which are labelled by their genre. The map nicely visualises the fact that many restaurants share common coordinates, since those coordinates refer to the area of the restaurant. Click on the single markers to see their air_store_id.")),
      tabItem(tabName = "BRD3_1",fluidRow(column(4, selectInput("storeid", "Select the Store ID: ", choices = unique(as.character(final_data$air_store_id)))), column(4, selectInput("Month", "Month", choices = c(as.numeric(sort(unique(final_data$month))))))), plotlyOutput("brd3"), h5("The above bar chart total visitor and total online reservation visitors with respect to Months. It allows to view visitors by store for months choosen from the dropdown")),
      tabItem(tabName = "BRD3_2",fluidRow(column(4, selectInput("storeid", "Select the Store ID: ", choices = unique(as.character(final_data$air_store_id)))),
                                        column(4, selectInput("Weekinput", "Week", c(sort(unique(final_data$week)))))), plotlyOutput("brd4"), h5("The above bar chart total visitor and total online reservation visitors with respect to Week. It allows to view visitors by store for weeks choosen from the dropdown")),
      
      tabItem(tabName = "BRD4", h4("Enter your location to find restaurants within 10km radius of your location:"),textInput("inputid","", "Tokyo"),h5("For example:, Tokya, Osaka, Fukuoka ..."),leafletOutput("map01",width = 1000 ,height = 900)),
      tabItem(tabName = "BRD5", fluidRow(selectInput("Area", "Area", choices = unique(as.character(final_data$location))), leafletOutput("gmap"),box(plotlyOutput("map1"),height = 250), box(plotlyOutput("map2"),height = 250))),
      tabItem(tabName = "BRD6", fluidRow(leafletOutput("mapOut"),
                                  
                                  absolutePanel(id = "controls", fixed = TRUE, draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                                width = 330, height = "auto",
                                                h2("Genre Exploration!"),
                                                selectInput("Genre", "Genre", choices = unique(as.character(final_data$air_genre_name))),
                                                plotlyOutput("bar1",height = 200)
                                                
                                  )
                                  
                                  
                                  
      ))
      #tabItem(tabName = "BRD5", selectInput("Year", "Select Year:", choices = as.numeric(sort(unique(final_data$year))), selectize = TRUE),selectInput("Month", "Month", choices = as.numeric(sort(unique(final_data$month))), selectize = TRUE),plotlyOutput("q3.2"))
    )
  )
)

server <- function(input, output, session) {
  
#  xdata <- reactive({
    
 #   xdata = sqldf("select air_store_id, sum(visitors) as total_visitors, sum(tot_res) as total_reservations, air_genre_name, air_area_name, latitude, longitude from final_data group by air_genre_name")
#    xdata
    
 # })
  
  output$brd1 <- renderPlotly({
    
    xdata = sqldf("select air_store_id, sum(visitors) as total_visitors, sum(tot_res) as total_reservations, air_genre_name, air_area_name, latitude, longitude from final_data group by air_genre_name")
    p <- plot_ly(xdata, x = ~air_genre_name, y = ~total_reservations, type = 'bar', name = 'Total Reservation Visitors') %>%
      add_trace(y = ~total_visitors, name = 'Total Visitors') %>%
      layout(yaxis = list(title = 'Total Visitors'), barmode = 'group')
    
    p
    
  })
  
  data <- reactive({
    
    new_data = sqldf("select air_store_id, visit_date, latitude, longitude, air_genre_name, air_area_name, sum(visitors) as total_visitors from final_data group by air_store_id")
    
    new_data
  })
  


  output$brd2 <- renderLeaflet({
    
    
    leaflet(data()) %>%
      addTiles() %>%
      addProviderTiles("CartoDB.Positron") %>%
      addMarkers(~longitude, ~latitude,
                 popup = paste("Total Customers: ",data()$total_visitors,"<br>", "Genre Name:",data()$air_genre_name, "<br>", "Area Name: ", data()$air_area_name), label = paste("store id: ", data()$air_store_id),
                 clusterOptions = markerClusterOptions())
    
  })
  
  mdata <- reactive({
    
    print("inside")
    hdata <- final_data %>% filter(air_store_id == input$storeid)
    hdata
    
  })
  
  
  output$brd4 <- renderPlotly({
    
    hdata <- final_data %>% filter(air_store_id == input$storeid)
    stmt = paste("select air_store_id, visit_date, week, month, sum(visitors) as total_visitors, sum(tot_res) as total_reservation_visitors from hdata where week between '01' and ",input$Weekinput, "group by  week")
    wdata = sqldf(stmt)
    
    print(wdata)
    
    p <- plot_ly(wdata, x = ~week, y = ~total_reservation_visitors, type = 'bar', name = 'Total Reservation Visitors') %>%
      add_trace(y = ~total_visitors, name = 'Total Visitors') %>%
      layout(yaxis = list(title = 'Total Visitors'), barmode = 'group')
    
    p
  })
  
  output$brd3 <- renderPlotly({
  
    hdata <- final_data %>% filter(air_store_id == input$storeid)
    stmt = paste("select air_store_id, month, sum(visitors) as total_visitors, sum(tot_res) as total_reservation_visitors from hdata where month between '01' and ",input$Month,"group by month")
    mdata = sqldf(stmt)
    
    
    p <- plot_ly(mdata, x = ~month, y = ~total_reservation_visitors, type = 'bar', name = 'Total Reservation Visitors') %>%
      add_trace(y = ~total_visitors, name = 'Total Visitors') %>%
      layout(yaxis = list(title = 'Total Visitors'), barmode = 'group')
    
    p
    
  })
  
  xdata <- reactive({
    
    hdata <- final_data %>% filter(year == input$Year)
    print(unique(hdata$year))
    hdata
    
  })
  
  observeEvent(
    input$Year,
    updateSelectInput(session, "Month", "Month", 
                      choices = xdata()$month[xdata()$year==input$Year]))
  
  
  a1 = reactive({input$Month})  
  
  output$q3.2 <- renderPlotly({
    hdata <- final_data %>% filter(year == input$Year)
    stmt = paste("select air_store_id, sum(visitors) as total_visitors, visit_date, month from hdata where month between '1' and ",input$Month, "group by month, air_store_id")
    Mdata = sqldf(stmt)
    Mdata$month = format(Mdata$visit_date,"%B")
    
    plot_ly(Mdata, x = ~month, y = ~total_visitors, type = "bar", name = "Total Visitors by MOnths") 
  })
  
  
  
  
  output$map01 <- renderLeaflet({
    
    
    a = geocode(input$inputid)
    points = data.frame(a)
    coord <-  data.frame(cbind(air_store$longitude, air_store$latitude))
    coord_df <- data.frame(coord, 
                           within_10km = geosphere::distHaversine(air_store[,5:4], c(points$lon, points$lat)) / 1000 < 10)    # convert m to km, check < 5
    
    df = data.frame(coord_df[ which(coord_df$within_10km=='TRUE'), ])
    
    
    # Show first 20 rows from the `quakes` dataset
    leaflet(data = df) %>% addTiles() %>%
      addMarkers( ~df[,1], ~df[,2],popup = paste("Air store id: ",as.character(air_store$air_store_id), "<br>", "Air Genre Name: ",as.character(air_store$air_genre_name)), label = paste("Area name:",as.character(air_store$air_area_name))
                  
      )
  })
  output$map1 <- renderPlotly({
    hdata <- final_data %>% filter(location == input$Area)
    area = sqldf("select air_store_id,count(distinct air_store_id) as Total_no_Restaurants, sum(visitors) as total_visitors, sum(tot_Res) as Resv_Visitors, location, latitude, longitude, air_genre_name from hdata group by location, air_genre_name")
    
    p <- plot_ly(area, x = ~location, y = ~Resv_Visitors, type = 'bar', name = 'Total Reservation Visitors') %>%
      add_trace(y = ~total_visitors, name = 'Total Visitors') %>%
      layout(yaxis = list(title = 'Total Visitors'), barmode = 'group')
    
    p
    
    
  })
  
  output$map2 <- renderPlotly({
    hdata <- final_data %>% filter(location == input$Area)
    area = sqldf("select air_store_id, count(distinct air_store_id) as Total_no_Restaurants, sum(visitors) as total_visitors, sum(tot_Res) as Resv_Visitors, air_area_name, latitude, longitude, air_genre_name from hdata group by air_area_name, air_genre_name")
    
    plot_ly(area, x = ~air_genre_name, y = ~Total_no_Restaurants, type = "bar") 
    
    
  })
  
  output$gmap <- renderLeaflet({
    
    hdata <- final_data %>% filter(location == input$Area)
    area = sqldf("select air_store_id, location, air_genre_name, count(distinct air_store_id) as Total_no_Restaurants, sum(visitors) as total_visitors, sum(tot_Res) as Resv_Visitors, latitude, longitude from hdata group by air_area_name, air_genre_name")
    
    
    leaflet(data = area) %>% addTiles() %>%
      addMarkers( ~longitude, ~latitude, popup = paste("Air store id: ",as.character(area$air_store_id), "<br>", "Location: ",as.character(area$location)), label = paste("Genre name:",as.character(area$air_genre_name)))
    
  })
  
  
  
  output$mapOut <- renderLeaflet({
    hdata <- final_data %>% filter(air_genre_name == input$Genre)
    genre = sqldf("select air_store_id, location, air_genre_name, count(distinct air_store_id) as Total_no_Restaurants, sum(visitors) as total_visitors, sum(tot_Res) as Resv_Visitors, latitude, longitude from hdata group by air_genre_name,location")
    
    
    leaflet(data = genre) %>% addTiles() %>%
      addMarkers( ~longitude, ~latitude, popup = paste("Air store id: ",as.character(genre$air_store_id), 
                                                       "Total No Restaurants: ", genre$Total_no_Restaurants, "<br>",
                                                       "Genre Name: ", as.character(genre$air_genre_name), "<br>",
                                                       "Location: ",as.character(genre$location)), label = paste("Genre name:",as.character(genre$air_genre_name)))
    
  })
  
  
  output$bar1 <- renderPlotly({
    hdata <- final_data %>% filter(air_genre_name == input$Genre)
    genre = sqldf("select air_store_id, location, air_genre_name, count(distinct air_store_id) as Total_no_Restaurants, sum(visitors) as total_visitors, sum(tot_Res) as Resv_Visitors, latitude, longitude from hdata group by air_genre_name,location")
    
    
    plot_ly(genre, x = ~location, y = ~Total_no_Restaurants, type = "bar")
    
  })
  
  
  output$trend <- renderPlotly({
    
    newdata <- merge(x=final_data, y=air_store, by="air_store_id")
    
  })
  
}

shinyApp(ui, server)


