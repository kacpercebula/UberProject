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

## Data Preparation :hammer:
1. Created pivot tables (grouped by team) for the six distinct stats involved in the Historical_Strength metric

---

## Data Analysis
- All the visualizations and explanations can be found on the shiny app link below
- https://kacpercebula.shinyapps.io/UberProject/

