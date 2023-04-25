# Uber Project :car:

Kacper Cebula, Visualizations shiny app url:
https://kacpercebula.shinyapps.io/UberProject/

## Introduction
- This project aims to analyze and visualize a dataset of Uber trips in New York City. The cleaning was simple. I needed to bind all the months data together, then convert the date/time column to be workable with filters. The analysis includes pivot tables, charts, heat maps, and a geospatial map created using the Leaflet library in Shiny. The charts display trips taken by hour, month, day, and base. Heat maps visualize trips by hour, day, month, week, and base. The geospatial map displays trip data on a map of New York City. All visualizations aim to provide insights into the trends and patterns of Uber trips in the city.
---

## Data Cleaning 
1. Imported necessary libraries to run visualizations and analysis.
2. Imported months data, saved it as rds, then made a data frame out of it (Example below).
```r
aug14 <- read.csv("uber-raw-data-aug14.csv")
saveRDS(aug14, "aug14.rds")
aug <- read_rds("aug14.rds")
```
3. Binded all the data into one data frame and set it to a data table.
```r
allMonths <- rbind(apr, may, jun, jul, aug, sep) 
setDT(allMonths)
```
4. Set the date and time format to be workable.
```r
allMonths$Date.Time <- as.POSIXct(allMonths$Date.Time, format = "%m/%d/%Y %H:%M:%S")
```

---

## Data Preparation 
- Trip Charts
1. Made a pivot table for specific visualization, used group_by and summarize as main pivot_table making functions.
```r
day_month_trips <- allMonths %>%
  group_by(month = month(Date.Time), day = day(Date.Time)) %>%
  summarize(trips = n()) %>%
  ungroup()
```
2. Made a ggplot using the pivot table to visualize the information in an appealing way.
```r
ggplot(day_month_trips, aes(x = day, y = trips, fill = factor(month))) +
  geom_bar(stat = "identity") +
  labs(x = "Day of Month", y = "Number of Trips") +
  facet_wrap(~ factor(month), ncol = 2, scales = "free_x") +
  scale_x_continuous(breaks = seq(1, 31, by = 3)) +
  scale_fill_discrete(name = "Month", labels = c("Apr", "May", "Jun", "Jul", "Aug", "Sep")) +
  theme_minimal()
```
 Heat Map - 
1. Made a pivot table for specific visualization, used group_by and summarize as main pivot_table making functions.
```r
base_weekday_heat <- allMonths %>%
  group_by(weekday = weekdays(Date.Time, abbreviate = TRUE), base = Base) %>%
  summarize(count = n()) 
```
2. Made a ggplot using the pivot table with geom_tile to visualize the information in an appealing way.
```r
ggplot(base_weekday_heat, aes(x = base, y = weekday, fill = count)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "purple") +
  labs(title = "Uber Trips Heatmap by Base and Weekday",
       x = "Base", y = "Day of the Week",
       fill = "Number of Trips") +
  theme_minimal()
```

Geospatial -
1. Made a smaller dataset of the original dataset so the map doesn't crash
2. Then made the map, set where it should view, and added markers using Lat, Lon, Date.Time and Base.
```r
subset_data <- allMonths[1:100,]

map <- leaflet(data = subset_data) %>% 
  addTiles() %>% 
  setView(lng = -73.9776, lat = 40.7588, zoom = 12) %>%
  addMarkers(lng = subset_data$Lon, lat = subset_data$Lat, 
             popup = paste0("Date.Time: ", subset_data$Date.Time, "<br>",
                            "Base: ", subset_data$Base))
```
 
---

## Shiny App
- All the visualizations and explanations of the charts can be found on the shiny app link below
- https://kacpercebula.shinyapps.io/UberProject/

