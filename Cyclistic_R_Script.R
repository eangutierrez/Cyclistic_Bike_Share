#----------------------------------------#
#-- CYCLISTIC BIKE-SHARE BUSINESS CASE --#
#----------------------------------------#
#   
# -- Author: Tony Gutierrez
# -- Date: 05/20/2023 
# -- Tools used: RStudio, R

# The purpose of this script is to create a data frame and load the raw data to the database management system for analysis

## Loading the Data ------------------------------------------------------------

### This code will set our working directory, load all pertinent libraries and clean the current environment from previously used variables and functions
#setwd("~/_Data_Analysis/_Projects_Directory/Cyclistic_Bike_Share/data/raw")

library(tidyverse)
library(tidylog)
library(janitor)
library(lubridate)
library(data.table)
library(filesstrings)
library(dplyr)
library(vtree)
library(RColorBrewer)
library(scales)
library(ggmap)
rm(list = ls())

### This code will read all the monthly bike share data and combine it into one file called Bike_Data_2022.csv for safekeeping purposes

files <- list.files(pattern = '.csv')
temp <- lapply(files, fread, sep= ",", fill = TRUE)
raw_df <- rbindlist(temp, fill = TRUE)
write.csv(raw_df, file = 'Bike_Data_2022.csv', row.names = FALSE)

### This code will move the csv file called Bike_Data_2022.csv to the interim data folder to begin pre-processing
file.move('Bike_Data_2022.csv', '../interim')

## Pre-processing the data -----------------------------------------------------

### This code will create a data frame called working_df in which we will carry out the data pre-processing.  It also renames all column labels to more appropriate ones
working_df <- raw_df %>% 
  rename("bike_type" = "rideable_type",
         "start_sta_name" = "start_station_name",
         "start_sta_id" = "start_station_id",
         "end_sta_name" = "end_station_name",
         "end_sta_id" = "end_station_id",
         "user_type" = "member_casual"
  )

### This code will delete all rows with null variables from the data frame
working_df <- working_df %>%
mutate(across(where(is.character), ~na_if(., ""))) %>% 
  drop_na

### This code will transform the data to the correct data types  
working_df <- working_df %>% 
  mutate(across(c("started_at", "ended_at"), mdy_hm))

### This code will find all duplicate record instances.  No further action needed as there were no duplicates
working_df %>% 
  group_by(ride_id, bike_type, started_at, ended_at, start_sta_name, end_sta_name) %>% 
  count(ride_id, bike_type, started_at, ended_at, start_sta_name, end_sta_name) %>% 
  filter(n > 1)

### This code will trim trailing and leading spaces in all columns with string values
working_df <- working_df %>%
  mutate(across(where(is.character), str_trim))

### This code will change all strings to Title Case
working_df <- working_df %>% 
  mutate(across(c(bike_type, start_sta_name, end_sta_name, user_type), stringr::str_to_title))

### This code will create a ride_length column, create a day_of_week column, and delete all records with a ride length < 1
working_df <- working_df %>% 
  mutate(ride_length = as.numeric(difftime(ended_at, started_at, unit = "mins")),
         day_of_week = lubridate::wday(started_at, label = TRUE, abbr = FALSE)) %>% 
  filter(ride_length >= 1) %>% 
  arrange(desc(ride_length))

## Analyzing the data ----------------------------------------------------------

### First, we will recreate the previously-built pivot tables using R code

### This code will find the average ride length in minutes by user type
working_df %>% 
  select(user_type, ride_length) %>% 
  group_by(user_type) %>%
  summarize(avg_ride_length_min = mean(ride_length)) %>% 
  ungroup() %>% 
  mutate(across(user_type, as.character)) %>% 
  bind_rows(summarize(user_type = "Combined", working_df, avg_ride_length_min = mean(ride_length)))

### This code will find the minimum ride length in minutes
working_df %>% 
  select(ride_length) %>% 
  summarize(min_ride_length_min = min(ride_length)) 

### This code will find the maximum ride length in minutes
working_df %>% 
  select(ride_length) %>% 
  summarize(max_ride_length_min = max(ride_length)) 

### This code will find the average ride length in minutes per user type per day of the week
working_df %>%
  mutate(across(c(user_type, day_of_week), as.character)) %>% 
  bind_rows(mutate(., user_type = "United")) %>% 
  bind_rows(mutate(., day_of_week = "gt")) %>% 
  group_by(user_type, day_of_week) %>% 
  summarize(avg_ride_length_min = mean(ride_length))  %>% 
  pivot_wider(names_from = day_of_week,
              values_from = avg_ride_length_min,
              names_prefix = "avg_ride_length_") %>% 
  select("user_type", "avg_ride_length_Monday", "avg_ride_length_Tuesday", "avg_ride_length_Wednesday", "avg_ride_length_Thursday", "avg_ride_length_Friday", "avg_ride_length_Saturday", "avg_ride_length_Sunday", "avg_ride_length_gt")

### This code will find the count number of rides per user type per day of the week
working_df %>%
  mutate(across(c(user_type, day_of_week), as.character)) %>% 
  bind_rows(mutate(., user_type = "United")) %>% 
  bind_rows(mutate(., day_of_week = "gt")) %>% 
  group_by(user_type, day_of_week) %>% 
  summarize(num_rides = n())  %>% 
  pivot_wider(names_from = day_of_week,
              values_from = num_rides,
              names_prefix = "num_rides_") %>% 
  select("user_type", "num_rides_Monday", "num_rides_Tuesday", "num_rides_Wednesday", "num_rides_Thursday", "num_rides_Friday", "num_rides_Saturday", "num_rides_Sunday", "num_rides_gt")

### This code will group all users into three categories based on ride_length, and calculate the number of users in that group per user type
working_df %>% 
  mutate(ride_levels = case_when(
    ride_length >= 1 & ride_length <= 10 ~ "short rides", 
    ride_length >= 11 & ride_length <= 20 ~ "medium rides",
    ride_length >= 21 ~ "long rides",
    TRUE ~ "Other"
  )) %>%
  mutate(across(c(ride_levels, user_type), as.character)) %>%
  bind_rows(mutate(., ride_levels = "all rides")) %>% 
  bind_rows(mutate(., user_type = "United")) %>% 
  group_by(ride_levels, user_type) %>%
  summarize(number_of_users = n()) %>% 
  select(ride_levels, user_type, number_of_users) %>% 
  arrange(desc(ride_levels))

### This code will count the number of rides per user type per month
working_df %>% 
  select(user_type, started_at) %>% 
  mutate(month_names = lubridate::month(started_at, label = TRUE, abbr = FALSE)) %>% 
  bind_rows(mutate(., month_names = "All Year")) %>% 
  bind_rows(mutate(., user_type = "United")) %>% 
  group_by(month_names, user_type) %>% 
  summarize(number_of_users = n()) %>% 
  arrange(factor(month_names, levels = c('January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December', 'All Year'))) %>% 
  print(n = 39)

### This code will find the top 10 most popular bike rides based on the start and end station names
working_df %>% 
  group_by(start_sta_name, end_sta_name) %>% 
  summarize(total_rides_occurred = n()) %>% 
  arrange(desc(total_rides_occurred)) %>%
  select(start_sta_name, end_sta_name, total_rides_occurred)
print(n = 10)

### This code will count the number of rides per hour of the day
working_df %>%
  mutate(hour_of_the_day = format(started_at, format = "%I %p" )) %>% 
  group_by(hour_of_the_day) %>% 
  summarize(total_rides = n()) %>% 
  ungroup() %>% 
  mutate(across(hour_of_the_day, as.character)) %>% 
  bind_rows(summarize(hour_of_the_day = "Total", working_df, total_rides = n())) %>% 
  arrange(factor(hour_of_the_day, levels = c('12 AM', '01 AM', '02 AM', '03 AM', '04 AM', '05 AM', '06 AM', '07 AM', '08 AM', '09 AM', '10 AM', '11 AM', '12 PM', '01 PM', '02 PM', '03 PM', '04 PM', '05 PM', '06 PM', '07 PM', '08 PM', '09 PM', '10 PM', '11 PM', 'Total'))) %>% 
  print(n = 25)

## Visualizing the data --------------------------------------------------------

### Now we can create powerful visualizations to convey our information more efficiently

### This code will create a tree distribution of the rides per user type and bike type
working_df %>%
  vtree(c("user_type", "bike_type"),
        palette = c(3, 5),
        horiz = FALSE,
        labelnode = list(bike_type = c(Classic = "Classic_bike",
                                       Docked = "Docked_bike",
                                       Electric = "Electric_bike")),
        labelvar = c(user_type = "Type of User", bike_type = "Type of Bike"))

### This code will create a visualization of the number of rides per ride length
ggplot(data = working_df, mapping = aes(x = ride_length)) +
  geom_histogram(binwidth = 1, color = "darkblue", fill = "lightblue") +
  scale_y_continuous(labels = label_number(suffix = " K", scale = 1e-3)) +
  coord_cartesian(xlim = c(0, 50)) +
  geom_hline(yintercept = 50000) +
  labs(x = "Ride Length (in Minutes)",
       y = "Number of Rides",
       title = "Number of Bike Rides Vs Ride Length Histogram",
       subtitle = "Frequency Distribution of Bike Rental Length in 2022",
       caption = "Horizontal Reference Line is Equal to 50 K Rides"
  )

### This code will create a line graph that shows distribution of rides per month
working_df %>%
  mutate(month_name = lubridate::month(started_at, label = TRUE, abbr = TRUE)) %>%
  select(user_type, month_name) %>%
  group_by(user_type, month_name) %>%
  summarize(total_rides = n()) %>%
  ggplot(mapping = aes(x = month_name, y = total_rides, color = user_type, group = user_type)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(labels = label_number(suffix = " K", scale = 1e-3)) +
  scale_color_brewer(palette = "Dark2") +
  geom_hline(yintercept = 50000) +
  labs(x = "Month",
       y = "Number of Rides",
       title = "Total Number of Rides Vs Month Line chart",
       subtitle = "Number of Rides per Month Compared Across User Type",
       caption = "Horizontal Reference Line is Equal to 50 K Rides",
       color = "User Type"
  )

### This code will create a visualization of the number of rides per day of the week per user type 
working_df %>%
  select(user_type, day_of_week) %>%
  ggplot(mapping = aes(x = factor(day_of_week, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")), fill = user_type)) +
  geom_bar(position = "dodge") +
  scale_y_continuous(labels = label_number(suffix = " K", scale = 1e-3)) +
  scale_fill_brewer(palette = "Dark2") +
  scale_x_discrete(
    labels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")) +
  labs(x = "Day of the Week",
       y = "Number of Rides",
       title = "Total Number of Rides Vs Day Bar Graph",
       subtitle = "Number of Rides per Day Compared Across User Type",
       fill = "User Type"
  )

### This code will create a visualization of heatmap of the number of rides per hour per day
working_df %>%
  mutate(hour_of_the_day = format(started_at, format = "%I %p" )) %>%
  group_by(day_of_week, hour_of_the_day) %>%
  summarize(total_rides = n()) %>%
  ggplot(mapping = aes(x = day_of_week, y = factor(hour_of_the_day, levels = c("11 PM", "10 PM", "09 PM", "08 PM", "07 PM", "06 PM", "05 PM", "04 PM", "03 PM", "02 PM", "01 PM", "12 PM", "11 AM", "10 AM", "09 AM", "08 AM", "07 AM", "06 AM", "05 AM", "04 AM", "03 AM", "02 AM", "01 AM", "12 AM" )), fill = total_rides)) +
  geom_tile() +
  scale_fill_gradient2(low = "#efedf5",
                       mid = "#bcbddc",
                       high = "#756bb1",
                       midpoint = 0) +
  scale_x_discrete(
    labels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
  ) +
  labs(x = "Day of the Week",
       y = "Hour of the Day",
       title = "Rides Across Hour of the Day Heatmap",
       subtitle = "Number of Rides per Hour Compared Across the Days of the Week",
       caption = "Horizontal Reference Line is Equal to 50,000 Rides",
       fill = "Total rides"
  )

### This code will create a visualization of a pie chart that shows weekday vs weekend usage per user_type
working_df %>%
  mutate(day_type = case_when(day_of_week %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday") ~ "WKDY",
                              day_of_week %in% c("Saturday", "Sunday") ~ "WKND")) %>%
  group_by(user_type, day_type) %>%
  summarize(total = n()) %>%
  arrange(desc(day_type)) %>%
  mutate(prop = total / sum(total) * 100) %>%
  mutate(ypos = cumsum(prop - 0.5*prop)) %>%
  ggplot(mapping = aes(x = "", y = prop, fill = day_type)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  facet_grid(~user_type) +
  theme_void() +
  theme(legend.position = "none") +
  geom_text(mapping = aes(y = ypos, label = day_type), color = "white", size = 6) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Rides in Workweek Vs Weekend",
       subtitle = "Number of Rides per User Type On the Workweek and Weekend"
  )

### This code will create a subset of the working data frame which filters out rides with the same starting and ending station, as well as the most popular trips Note: popular is calculated as any trips with greater than or equal to 250 occurrences
mc_mapcoords <- working_df %>%
  filter(start_sta_name != end_sta_name) %>%
  group_by(start_lat, start_lng, end_lat, end_lng, user_type) %>%
  summarize(total_rides = n()) %>%
  filter(total_rides >= 250) %>% 
  mutate(category = case_when(
    total_rides <= 500 ~ "< 500 Times",
    total_rides >= 501 ~ "> 500 Times"
  )) 

### This code will create two additional subsets that filter out member and casual users, respectively
mc_member <- mc_mapcoords %>%
  filter(user_type == "Member")

mc_casual <- mc_mapcoords %>% 
  filter(user_type == "Casual")

### This code will create a window of coordinates for the map
map_window <- c(
  top = 41.990119,
  right = -87.554855,
  bottom = 41.790769,
  left = -87.700424
)

### This code will format our map
project_stamen <- get_stamenmap(
  bbox = map_window,
  zoom = 12,
  maptype = "toner"
)

### This code will create two maps that show the most popular trips for member and casual users, respectively
ggmap(project_stamen, darken = c(0.8, "white")) +
  geom_curve(data = mc_member, mapping = aes(x = start_lng, y = start_lat, xend = end_lng, yend = end_lat, alpha = category, color = category), size = 0.5, curvature = 0.2, arrow = arrow(length = unit(0.2, "cm"), ends = "first", type = "closed")) +
  scale_color_manual(breaks = c("< 500 Times", "> 500 Times"),
                     values = c("violet", "deepskyblue")) +
  scale_alpha_discrete(range = c(0.2, 0.8), guide = "none") +
  coord_cartesian() +
  labs(
    title = "Most Popular Routes by Member Users", 
    x = NULL,
    y = NULL,
    color = "User Type",
    caption = "Data provided by Google Data Analytics Certificate Course") 

ggmap(project_stamen, darken = c(0.8, "white")) +
  geom_curve(data = mc_casual, mapping = aes(x = start_lng, y = start_lat, xend = end_lng, yend = end_lat, alpha = category, color = category), size = 0.5, curvature = 0.2, arrow = arrow(length = unit(0.2, "cm"), ends = "first", type = "closed")) +
  scale_color_manual(breaks = c("< 500 Times", "> 500 Times"),
                     values = c("violet", "deepskyblue")) +
  scale_alpha_discrete(range = c(0.4, 0.8), guide = "none") +
  coord_cartesian() +
  labs(
    title = "Most Popular Routes by Casual Users", 
    x = NULL,
    y = NULL,
    color = "User Type",
    caption = "Data provided by Google Data Analytics Certificate Course") 
