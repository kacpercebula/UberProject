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

#inputing data
# aug14 <- read.csv("uber-raw-data-aug14.csv")
# jul14 <- read.csv("uber-raw-data-jul14.csv")
# jun14 <- read.csv("uber-raw-data-jun14.csv")
# may14 <- read.csv("uber-raw-data-may14.csv")
# sep14 <- read.csv("uber-raw-data-sep14.csv")
# apr14 <- read.csv("uber-raw-data-apr14.csv")
# 
# saveRDS(aug14, "aug14.rds")
# saveRDS(jul14, "jul14.rds")
# saveRDS(jun14, "jun14.rds")
# saveRDS(may14, "may14.rds")
# saveRDS(sep14, "sep14.rds")
# saveRDS(apr14, "apr14.rds")

# aug <- read_rds("aug14.rds")
# jul <- read_rds("jul14.rds")
# jun <- read_rds("jun14.rds")
# may <- read_rds("may14.rds")
# sep <- read_rds("sep14.rds")
# apr <- read_rds("apr14.rds")
# 
# allMonths <- rbind(apr, may, jun, jul, aug, sep) 
# setDT(allMonths)
# 
# saveRDS(allMonths, "allMonths.rds")

allMonths <- read_rds("allMonths.rds")

#set date and time format.
allMonths$Date.Time <- as.POSIXct(allMonths$Date.Time, format = "%m/%d/%Y %H:%M:%S")
## end of cleaning


###Pivot table to display trips by the hour.
hour_trips <- allMonths %>%
  group_by(hour = hour(Date.Time)) %>%
  summarize(trips = n())
#DONE


###Chart that shows Trips by Hour and Month
month_hour_trips <- allMonths %>%
  group_by(month = month(Date.Time), hour = hour(Date.Time)) %>%
  summarize(trips = n()) %>%
  ungroup()
ggplot(month_hour_trips, aes(x = hour, y = trips, color = factor(month))) +
  geom_line() +
  scale_x_continuous(breaks = 0:23) +
  scale_color_discrete(name = "Month", labels = c("Apr", "May", "Jun", "Jul", "Aug", "Sep")) +
  labs(x = "Hour of Day", y = "Number of Trips", color = "Month") +
  theme_minimal()
#DONE


###Chart that displays Trips Every Hour.
ggplot(hour_trips, aes(x = hour, y = trips)) +
  geom_line() +
  scale_x_continuous(breaks = 0:23) +
  labs(x = "Hour of Day", y = "Number of Trips") +
  theme_minimal()
#DONE


###Plot data by trips taken during every day of the month.
day_month_trips <- allMonths %>%
  group_by(month = month(Date.Time), day = day(Date.Time)) %>%
  summarize(trips = n()) %>%
  ungroup()
ggplot(day_month_trips, aes(x = day, y = trips, fill = factor(month))) +
  geom_bar(stat = "identity") +
  labs(x = "Day of Month", y = "Number of Trips") +
  facet_wrap(~ factor(month), ncol = 2, scales = "free_x") +
  scale_x_continuous(breaks = seq(1, 31, by = 3)) +
  scale_fill_discrete(name = "Month", labels = c("Apr", "May", "Jun", "Jul", "Aug", "Sep")) +
  theme_minimal()
#DONEish

#I should see a table that shows Trips Every Day (Max 31 days in a month so I should see total trips taken each day). 
day_trips <- allMonths %>%
  group_by(day = day(Date.Time)) %>%
  summarize(trips = n()) %>%
  ungroup()
ggplot(day_trips, aes(x = day, y = trips)) +
  geom_bar(stat = "identity", fill = "lavender", color = "black") +
  scale_x_continuous(breaks = seq(1, 31, by = 3)) +
  theme(axis.text.x = element_text(size = 7, angle = 35)) +
  labs(x = "Day of month", y = "Number of Trips") +
  theme_minimal()
#DONE


#Chart by Trips by Day and Month (bar chart with each day of the week, x axis as the month). I need a chart that shows number of trips by month
weekday_trips <- allMonths %>%
  mutate(weekday = weekdays(Date.Time,abbreviate = TRUE)) %>%
  group_by(month = month(Date.Time), weekday) %>%
  summarize(trips = n()) %>%
  ungroup() %>%
  mutate(weekday = factor(weekday, levels = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")))
ggplot(weekday_trips, aes(x = weekday, y = trips, fill = factor(month))) +
  geom_bar(stat = "identity") +
  labs(x = "Day of Week", y = "Number of Trips") +
  facet_wrap(~ factor(month), ncol = 2, scales = "free_x") +
  scale_fill_discrete(name = "Month", labels = c("Apr", "May", "Jun", "Jul", "Aug", "Sep")) +
  theme_minimal()
#DONE

###I need a chart that shows number of trips by month
month_trips <- allMonths %>%
  group_by(month = month(Date.Time)) %>%
  summarize(trips = n()) %>%
  ungroup()
ggplot(month_trips, aes(x = factor(month), y = trips)) +
  geom_bar(stat = "identity", fill = "#69b3a2") +
  labs(x = "Month", y = "Number of Trips") +
  scale_x_discrete(labels = c("Apr", "May", "Jun", "Jul", "Aug", "Sep")) +
  theme_minimal()


#Chart Trips by Bases and Month (Base is the X axis and Month is your label)
base_month_trips <- allMonths %>%
  group_by(base = Base, month = month(Date.Time)) %>%
  summarize(trips = n()) %>%
  ungroup()
ggplot(base_month_trips, aes(x = base, y = trips, fill = factor(month))) +
  geom_bar(stat = "identity") +
  labs(x = "Day of Month", y = "Number of Trips") +
  facet_wrap(~ factor(month), ncol = 2, scales = "free_x", labeller = ) +
  scale_fill_discrete(name = "Month", labels = c("Apr", "May", "Jun", "Jul", "Aug", "Sep")) +
  theme_minimal()



###Heat Maps

#Heat map that displays by hour and day
hour_day_heat <- allMonths %>%
  group_by(day = day(Date.Time), hour = hour(Date.Time)) %>%
  summarize(count = n())

ggplot(hour_day_heat, aes(x = hour, y = day, fill = count)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "steelblue") +
  scale_x_continuous(breaks = seq(0, 23, by = 2)) +
  scale_y_continuous(breaks = seq(1, 31, by = 2)) +
  labs(title = "Uber Trips Heatmap by Hour and Day",
       x = "Hour of Day", y = "Day of Month",
       fill = "Number of Trips") +
  theme_minimal()

#Heat map by month and day
month_day_heat <- allMonths %>%
  group_by(day = day(Date.Time), month = month(Date.Time)) %>%
  summarize(count = n()) 

ggplot(month_day_heat, aes(x = month, y = day, fill = count)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  scale_x_continuous(breaks = c(4, 5, 6, 7, 8, 9),
                     labels = c("Apr", "May", "Jun", "Jul", "Aug", "Sep")) +
  scale_y_continuous(breaks = seq(1, 31, by = 2)) +
  labs(title = "Uber Trips Heatmap by Month and Day",
       x = "Month", y = "Day of Month",
       fill = "Number of Trips") +
  theme_minimal()

#Heat map by month and week
month_weekday_heat <- allMonths %>%
  group_by(weekday = weekdays(Date.Time, abbreviate = TRUE), month = month(Date.Time)) %>%
  summarize(count = n()) %>%
  mutate(weekday = factor(weekday, levels = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"))) 

ggplot(month_weekday_heat, aes(x = month, y = weekday, fill = count)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "green") +
  scale_x_continuous(breaks = c(4, 5, 6, 7, 8, 9),
                    labels = c("Apr", "May", "Jun", "Jul", "Aug", "Sep")) +
  labs(title = "Uber Trips Heatmap by Month and Weekday",
       x = "Month", y = "Day of the Week",
       fill = "Number of Trips") +
  theme_minimal()

#Heat map Bases and Day of Week
base_weekday_heat <- allMonths %>%
  group_by(weekday = weekdays(Date.Time, abbreviate = TRUE), base = Base) %>%
  summarize(count = n()) 

ggplot(base_weekday_heat, aes(x = base, y = weekday, fill = count)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "purple") +
  labs(title = "Uber Trips Heatmap by Base and Weekday",
       x = "Base", y = "Day of the Week",
       fill = "Number of Trips") +
  theme_minimal()


#Leaflet Shiny Geospatial Map this portion is subject to change. 
subset_data <- allMonths[1:100,]

# create map with initial center and zoom level
map <- leaflet(data = subset_data) %>% 
  addTiles() %>% 
  setView(lng = -73.9776, lat = 40.7588, zoom = 12)

# add markers with popups
map <- map %>% 
  addMarkers(lng = subset_data$Lon, lat = subset_data$Lat, 
             popup = paste0("Date.Time: ", subset_data$Date.Time, "<br>",
                            "Base: ", subset_data$Base))

# display map
map


#Saving the pivot tables

saveRDS(base_month_trips, "base_month_trips.rds")
saveRDS(day_month_trips, "day_month_trips.rds")
saveRDS(day_trips, "day_trips.rds")
saveRDS(hour_trips, "hour_trips.rds")
saveRDS(month_hour_trips, "month_hour_trips.rds")
saveRDS(month_trips, "month_trips.rds")
saveRDS(subset_data, "subset_data.rds")
saveRDS(weekday_trips, "weekday_trips.rds")
saveRDS(base_weekday_heat, "base_weekday_heat.rds")
saveRDS(month_day_heat, "month_day_heat.rds")
saveRDS(hour_day_heat, "hour_day_heat.rds")
saveRDS(month_weekday_heat, "month_weekday_heat.rds")






