#library(tidytext)
#library(textdata)
library(dplyr)
#library(stringr)
library(tidyr)
library(ggplot2)
#library(wordcloud)
#library(sentimentr)
#library(reshape2)
library(readr)
library(shiny)
library(DT)
library(lubridate)
library(tidyverse)
library(data.table)
library(leaflet)

rm(list = ls())

ui <- fluidPage(
  tabsetPanel(
    tabPanel(
      "Hourly Trips by Month",
      plotOutput("Hour/Trips months"),
      p("Hourly Trips by Month: This chart shows the number of trips taken by hour, 
        grouped by month. The x-axis shows the hour of the day, and the y-axis shows 
        the number of trips. The different colored lines represent each month. You can 
        use this chart to see how the number of trips taken during each hour changes over 
        the course of the year.")
    ),
    tabPanel(
      "Hourly Trips (Total) / Prediction model",
      plotOutput("Hour/Trips total"),
      p("Hourly Trips (Total) / Prediction model is a chart that shows the total number of trips 
        taken by hour, regardless of month. The x-axis displays the hour of the day, and the 
        y-axis shows the number of trips. This chart can be used to analyze how the number of 
        trips taken varies throughout the day. Additionally, the chart can be used to predict the 
        number of trips that can be taken during each hour of the day based on past data. This can 
        be useful for optimizing your travel plans and avoiding peak hours with heavy traffic.")
    ),
    tabPanel(
      "Trips by Month & Day",
      plotOutput("Monthday/Trips months"),
      p("Trips by Month & Day: This chart shows the number of trips taken by day, grouped by 
        month. The x-axis shows the day of the month, and the y-axis shows the number of trips. 
        The different colored lines represent each month. You can use this chart to see how 
        the number of trips taken on each day of the month changes over the course of the year.")
    ),
    tabPanel(
      "Trips by Month & Day (Total)",
      plotOutput("Monthday/Trips total"),
      p("Trips by Month & Day (Total): This chart shows the total number of trips taken by day, 
        regardless of month. The x-axis shows the day of the month, and the y-axis shows the 
        number of trips. You can use this chart to see how the number of trips taken on each day 
        of the month varies throughout the year.")
    ),
    tabPanel(
      "Trips by Month & Weekday ",
      plotOutput("Week/Trips weekday"),
      p("Trips by Month & Weekday: This chart shows the number of trips taken by weekday, grouped 
        by month. The x-axis shows the weekday, and the y-axis shows the number of trips. The 
        different colored lines represent each month. You can use this chart to see how the number 
        of trips taken on each weekday changes over the course of the year.")
    ),
    tabPanel(
      "Trips by Month & Weekday (Total)",
      plotOutput("Month/Trips total"),
      p("Trips by Month & Weekday (Total): This chart shows the total number of trips taken 
      by weekday, regardless of month. The x-axis shows the weekday, and the y-axis shows the 
      number of trips. You can use this chart to see how the number of trips taken on each weekday 
      varies throughout the year.")
    ),
    tabPanel(
      "Trips by Base & Month",
      plotOutput("Base/Trips months"),
      p("Trips by Base & Month: This chart shows the number of trips taken by base, grouped by month. 
        The x-axis shows the base, and the y-axis shows the number of trips. The different colored bars 
        represent each month. You can use this chart to see which bases have the highest number of trips 
        each month.")
    ),
    tabPanel(
      "Heatmap by Hour and Day",
      plotOutput("hour/day"),
      p("Heatmap by Hour and Day: This heatmap shows the number of trips taken by hour and day. The x-axis 
        shows the hour of the day, and the y-axis shows the day of the month. The color of each tile 
        represents the number of trips taken at that hour and day. You can use this heatmap to see which 
        hours and days of the month are busiest for Uber trips.")
    ),
    tabPanel(
      "Heatmap by Month and Day",
      plotOutput("month/day"),
      p("Heatmap by Month and Day: This heatmap shows the number of trips taken by month and day. The x-axis 
        shows the month, and the y-axis shows the day of the month. The color of each tile represents the 
        number of trips taken on that day of the month. You can use this heatmap to see which days of the 
        month are busiest for Uber trips.")
    ),
    tabPanel(
      "Heatmap by Month and Weekday",
      plotOutput("month/weekday"),
      p("Heatmap by Month and Weekday: This heatmap shows the number of trips taken by month and weekday. 
        The x-axis shows the month, and the y-axis shows the weekday. The color of each tile represents the 
        number of trips taken on that weekday. You can use this heatmap to see which weekdays are busiest for
        Uber trips each month.")
    ),
    tabPanel(
      "Heatmap by Base and Weekday",
      plotOutput("base/weekday"),
      p("Heatmap by Base and Weekday: This heatmap shows the number of trips taken by base and weekday. 
        The x-axis shows the base, and the y-axis shows the weekday. The color of each represents the 
        number of trips taken on that weekday. You can use this heatmap to see which weekdays are busiest for
        Uber trips at each base.")
    ),
    tabPanel(
      "Geospatial Leaflet",
      leafletOutput("geoMap"),
      p("GeoSpatial Leaflet: This geospatial leaflest shows a sample of size of 100 from the original raw 
        data set of the date, time, lat, lon, and base. It uses the information provided from the data set
        to pin point markers that show the date, time and base at which they were taken. You can use this leaflet
        to see where it busiet on Uber trips.")
    )
  )
)

server <- function(input, output) {
  
  #Read in the data
  base_month_trips <- read_rds("base_month_trips.rds")
  day_month_trips <- read_rds("day_month_trips.rds")
  day_trips <- read_rds("day_trips.rds")
  hour_trips <- read_rds("hour_trips.rds")
  month_hour_trips <- read_rds("month_hour_trips.rds")
  month_trips <- read_rds("month_trips.rds")
  subset_data <- read_rds("subset_data.rds")
  weekday_trips <- read_rds("weekday_trips.rds")
  base_weekday_heat <-read_rds("base_weekday_heat.rds")
  month_day_heat <- read_rds("month_day_heat.rds")
  hour_day_heat <- read_rds("hour_day_heat.rds")
  month_weekday_heat <- read_rds("month_weekday_heat.rds")
  
  #Charts
  output$`Hour/Trips months` <- renderPlot({
    ggplot(month_hour_trips, aes(x = hour, y = trips, color = factor(month))) +
      geom_line() +
      scale_x_continuous(breaks = 0:23) +
      scale_color_discrete(name = "Month", labels = c("Apr", "May", "Jun", "Jul", "Aug", "Sep")) +
      labs(x = "Hour of Day", y = "Number of Trips", color = "Month") +
      theme_minimal()
  })
  
  output$`Hour/Trips total` <- renderPlot({
    ggplot(hour_trips, aes(x = hour, y = trips)) +
      geom_line() +
      scale_x_continuous(breaks = 0:23) +
      labs(x = "Hour of Day", y = "Number of Trips") +
      theme_minimal()
  })
  
  output$`Monthday/Trips months` <- renderPlot({
    ggplot(day_month_trips, aes(x = day, y = trips, fill = factor(month))) +
      geom_bar(stat = "identity") +
      labs(x = "Day of Month", y = "Number of Trips") +
      facet_wrap(~ factor(month), ncol = 2, scales = "free_x") +
      scale_x_continuous(breaks = seq(1, 31, by = 3)) +
      scale_fill_discrete(name = "Month", labels = c("Apr", "May", "Jun", "Jul", "Aug", "Sep")) +
      theme_minimal()
  })
  
  output$`Monthday/Trips total` <- renderPlot({
    ggplot(day_trips, aes(x = day, y = trips)) +
      geom_bar(stat = "identity", fill = "lavender", color = "black") +
      scale_x_continuous(breaks = seq(1, 31, by = 3)) +
      theme(axis.text.x = element_text(size = 7, angle = 35)) +
      labs(x = "Day of month", y = "Number of Trips") +
      theme_minimal()
  })
  
  output$`Week/Trips weekday` <- renderPlot({
    ggplot(weekday_trips, aes(x = weekday, y = trips, fill = factor(month))) +
      geom_bar(stat = "identity") +
      labs(x = "Day of Week", y = "Number of Trips") +
      facet_wrap(~ factor(month), ncol = 2, scales = "free_x") +
      scale_fill_discrete(name = "Month", labels = c("Apr", "May", "Jun", "Jul", "Aug", "Sep")) +
      theme_minimal()
  })
  
  
  output$`Month/Trips total` <- renderPlot({
    ggplot(month_trips, aes(x = factor(month), y = trips)) +
      geom_bar(stat = "identity", fill = "#69b3a2") +
      labs(x = "Month", y = "Number of Trips") +
      scale_x_discrete(labels = c("Apr", "May", "Jun", "Jul", "Aug", "Sep")) +
      theme_minimal()
  })
  
  output$`Base/Trips months` <- renderPlot({
    ggplot(base_month_trips, aes(x = base, y = trips, fill = factor(month))) +
      geom_bar(stat = "identity") +
      labs(x = "Day of Month", y = "Number of Trips") +
      facet_wrap(~ factor(month), ncol = 2, scales = "free_x", labeller = ) +
      scale_fill_discrete(name = "Month", labels = c("Apr", "May", "Jun", "Jul", "Aug", "Sep")) +
      theme_minimal()
  })
  
  #Heat Maps
  output$`hour/day` <- renderPlot({
    ggplot(hour_day_heat, aes(x = hour, y = day, fill = count)) +
      geom_tile() +
      scale_fill_gradient(low = "white", high = "steelblue") +
      scale_x_continuous(breaks = seq(0, 23, by = 2)) +
      scale_y_continuous(breaks = seq(1, 31, by = 2)) +
      labs(x = "Hour of Day", y = "Day of Month",
           fill = "Number of Trips") +
      theme_minimal()
  })
  
  output$`month/day` <- renderPlot({
    ggplot(month_day_heat, aes(x = month, y = day, fill = count)) +
      geom_tile() +
      scale_fill_gradient(low = "white", high = "red") +
      scale_x_continuous(breaks = c(4, 5, 6, 7, 8, 9),
                         labels = c("Apr", "May", "Jun", "Jul", "Aug", "Sep")) +
      scale_y_continuous(breaks = seq(1, 31, by = 2)) +
      labs(x = "Month", y = "Day of Month",
           fill = "Number of Trips") +
      theme_minimal()
  })
  
  output$`month/weekday` <- renderPlot({
    ggplot(month_weekday_heat, aes(x = month, y = weekday, fill = count)) +
      geom_tile() +
      scale_fill_gradient(low = "white", high = "green") +
      scale_x_continuous(breaks = c(4, 5, 6, 7, 8, 9),
                         labels = c("Apr", "May", "Jun", "Jul", "Aug", "Sep")) +
      labs(x = "Month", y = "Day of the Week",
           fill = "Number of Trips") +
      theme_minimal()
  })
  
  output$`base/weekday` <- renderPlot({
    ggplot(base_weekday_heat, aes(x = base, y = weekday, fill = count)) +
      geom_tile() +
      scale_fill_gradient(low = "white", high = "purple") +
      labs(x = "Base", y = "Day of the Week",
           fill = "Number of Trips") +
      theme_minimal()
  })
  
  output$`geoMap` <- renderLeaflet ({
    leaflet(data = subset_data) %>% 
      addTiles() %>% 
      setView(lng = -73.9776, lat = 40.7588, zoom = 12) %>%
      addMarkers(lng = subset_data$Lon, lat = subset_data$Lat, 
                 popup = paste0("Date.Time: ", subset_data$Date.Time, "<br>",
                                "Base: ", subset_data$Base))
  })
  
  
}

shinyApp(ui=ui, server=server)



