library(ggplot2)
library(tidyverse)
library(lubridate)
library(wesanderson)
library(scales)

ch <- read.csv("C:/Users/rlehrter/Desktop/R Working Directory/Bikeshare/chicago.csv")
ny <- read.csv("C:/Users/rlehrter/Desktop/R Working Directory/Bikeshare/new-york-city.csv")
wa <- read.csv("C:/Users/rlehrter/Desktop/R Working Directory/Bikeshare/washington.csv")

ch <- ch %>%
  add_column(city = "Chicago") %>%
  mutate(trip_minutes = Trip.Duration/60)

ny <- ny %>%
  add_column(city = "New York") %>%
  mutate(trip_minutes = Trip.Duration/60)

wa <- wa %>%
  add_column(city = "Washington") %>%
  mutate(trip_minutes = Trip.Duration/60)

bikes <- ch %>% 
  full_join(ny) %>% 
  full_join(wa)

#3 cities joined into one table, some columns missing data-
#washington has no gender or birth year data

###PROJECT QUESTIONS###

#1 Popular times of travel (i.e., occurs most often in the start time)
  #What is the most common month?
  #What is the most common day of week?
  #What is the most common hour of day?  

#Split Dates#

bikes <- bikes %>%
  dplyr::mutate(year = lubridate::year(Start.Time), 
                month = lubridate::month(Start.Time), 
                day = lubridate::day(Start.Time),
                hour = lubridate::hour(Start.Time),
                minute = lubridate::minute(Start.Time),
                second = lubridate::second(Start.Time))

summary(bikes)

#Plot by month
ggplot(bikes, aes(x = month, fill = city)) +
  geom_bar(position = "dodge") +
  labs(title = "Bikeshare Usage Data by Month for 3 Major U.S. Cities", subtitle = "2017", x = "Calendar Month", y = "Number of Bike Rentals", fill = "City") +
  scale_x_continuous(breaks=seq(0,6,1)) +
  scale_fill_manual(values = wes_palette(name = "Darjeeling1")) +
  theme_minimal()

#Plot by hour
ggplot(bikes, aes(x = hour, fill = city)) +
  geom_bar(position = "dodge") +
  labs(title = "Cumulative Bikeshare Usage Data by Hour for 3 Major U.S. Cities Over 6 Months", subtitle = "2017", x = "Hour of Day", y = "Number of Bike Rentals", fill = "City") +
  scale_x_continuous(breaks=seq(0, 23, 1)) +
  scale_fill_manual(values = wes_palette(name = "Darjeeling1")) +
  theme_minimal()


#2 Popular stations and trip
#What is the most common start station?
#What is the most common end station?
#What is the most common trip from start to end (i.e., most frequent combination of start station and end station)?

#Create a "mode" function for ease of calculating "most common"
get_mode <- function(x){
  return(names(sort(table(x), decreasing = T, na.last = T)[1]))
}

ch$trips <- paste(ch$Start.Station, "to", ch$End.Station)
ny$trips <- paste(ny$Start.Station, "to", ny$End.Station)
wa$trips <- paste(wa$Start.Station, "to", wa$End.Station)

get_mode(ch$Start.Station)
get_mode(ch$End.Station)
get_mode(ch$trips)
get_mode(ny$Start.Station)
get_mode(ny$End.Station)
get_mode(ny$trips)
get_mode(wa$Start.Station)
get_mode(wa$End.Station)
get_mode(wa$trips)

bikes$trips <- paste(bikes$Start.Station, "to", bikes$End.Station)


#3 Trip duration
#What is the total travel time for users in different cities?
#What is the average travel time for users in different cities?

ggplot(bikes, aes(x = city, y = Trip.Duration)) +
  geom_boxplot(position = "dodge") +
  labs(title = "Bikeshare Usage Data by Month for 3 Major U.S. Cities", subtitle = "2017", x = "Calendar Month", y = "Number of Bike Rentals", fill = "City") +
  scale_fill_manual(values = wes_palette(name = "Darjeeling1")) +
  theme_minimal()

#looks like there are some outliers in trip duration, likely erroneous values or bikes kept for extended periods of time

#Let's try plotting again, taking this into account
ggplot(bikes, aes(x = city, y = trip_minutes, fill = city)) +
  geom_boxplot(position = "dodge") +
  labs(title = "Bikeshare Trip Length for 3 Major U.S. Cities", subtitle = "2017", x = "City", y = "Trip Duration (minutes)", fill = "City") +
  scale_fill_manual(values = wes_palette(name = "Darjeeling1")) +
  stat_summary(fun=mean, geom="point", shape=20, size=10, color="black", fill="white") +
  coord_cartesian(ylim = c(0, 60)) +
  theme_minimal()

#Calculate summary stats for each column to answer question with values
summary(ch)
summary(ny)
summary(wa)
