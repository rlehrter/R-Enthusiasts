##code for making a "wind rose" windspeed visualization using NEON data from the API##

library(ggplot2)
library(tidyverse)
library(rvest)
library(neonUtilities)
library(dplyr)

#load 2D wind data for NIWO using API, wait for prompt & hit y to download data#
wind <- loadByProduct(dpID='DP1.00001.001', site = c("NIWO"), package = "basic")

#view structure of file
View(wind)
View(wind$`2DWSD_30min`)

#subset for needed variables
NIWO_wind <- (wind$'2DWSD_30min'[c("siteID", "windSpeedMean", "windDirMean")])

#attach function makes the file name "searchable" by R#
attach(NIWO_wind)


#attempt to convert degrees to cardinal directions (factor)#
#first make directions table by pulling some code from the internet#

url <- 'http://snowfence.umn.edu/Components/winddirectionanddegreeswithouttable3.htm'
page <- read_html(url)
directions_raw <- page %>% html_node('td table') %>% html_table(header = TRUE)

directions <- directions_raw %>% 
  set_names(~tolower(sub(' Direction', '', .x))) %>% 
  slice(-1) %>% 
  separate(degree, c('degree_min', 'degree_max'), sep = '\\s+-\\s+', convert = TRUE)

NIWO_wind_dir <- NIWO_wind %>% 
  mutate(wd_cardinal = cut(
    windDirMean, 
    breaks = c(0, directions$degree_max, 360), 
    labels = c(directions$cardinal, 'N')
  ))
#we now have cardinal directions in NIWO_wind_dir#
str(NIWO_wind_dir)
summary(NIWO_wind_dir)

#onto mutating and binning windspeeds#

#convert windspeed from m/s to mph using mutate function and the conversion factor#
#add new variable windSpeedMPH#
NIWO_wind_dir <- NIWO_wind_dir %>%
  mutate(windSpeedMPH = windSpeedMean * 2.2369362920544) 

#choose breaks for binning windspeed values#
breaks <- c(0,10,20,30,40,50,75)

#group together measurements of chosen breaks using cut function#
NIWO_wind_dir_binned <- NIWO_wind_dir %>% 
  mutate(windspeed_bins = cut(windSpeedMPH, breaks = breaks))

view(NIWO_wind_dir_binned)

#write over old file to make things easier#
NIWO_wind <- NIWO_wind_dir_binned

#omit NAs for proper plotting#
NIWO_wind <- na.omit(NIWO_wind)

##wind rose plot using ggplot##
# Using wind, plot wd filled by ws
ggplot(NIWO_wind, aes(wd_cardinal, fill = windspeed_bins)) +
  # Add a bar layer with width 1
  geom_bar(width = 1)

# Convert to polar coordinates:
ggplot(NIWO_wind, aes(wd_cardinal, fill = windspeed_bins)) +
  geom_bar(width = 1) +
  coord_polar()
# align the axis properly:
NIWO_windrose_dirty <- ggplot(NIWO_wind, aes(wd_cardinal, fill = windspeed_bins)) +
  geom_bar(width = 1) +
  coord_polar(start = -pi/16)
print(NIWO_windrose_dirty)

#make it look nice#
NIWO_windrose <- NIWO_windrose_dirty +
  scale_fill_discrete(name = "Windspeed (MPH)", labels = c("0-10", "10-20", "20-30", "30-40", "40-50", "50 +")) +
  labs(x = "Wind Direction", y = "Number of Observations", size = 20) +
  ggtitle("NIWO 5-Year Cumulative Windspeeds (30 min intervals)") +
  theme_minimal()
print(NIWO_windrose)

NIWO_windrose

#y-axis log10 transformation#
NIWO_windrose + scale_y_log10()