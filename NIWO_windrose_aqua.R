##code for making a "wind rose" windspeed visualizations of D1013 Aqua Sites using NEON data from the API##
#RJL 2021#

library(ggplot2)
library(tidyverse)
library(rvest)
library(neonUtilities)
library(dplyr)

#load 2D wind data for aqua sites using API, wait for prompt & hit y to download data#
wind_master <- neonUtilities::loadByProduct(
  dpID="DP1.00001.001",# The data product ID for surface water chemistry. this can be changed to any data product ID
  check.size = F,# This input will force the function to download the data no matter the size of the download
  site = c("ARIK", "COMO", "WLOU") # This is the site you specified above
)

#view structure of file
View(wind)
View(wind$`2DWSD_30min`)

#subset for needed variables
wind <- (wind_master$'twoDWSD_30min'[c("siteID", "windSpeedMean", "windDirMean")])

#attach function makes the file name "searchable" by R#
attach(wind)


#attempt to convert degrees to cardinal directions (factor)#
#first make directions table by pulling some code from the internet#

url <- 'http://snowfence.umn.edu/Components/winddirectionanddegreeswithouttable3.htm'
page <- read_html(url)
directions_raw <- page %>% html_node('td table') %>% html_table(header = TRUE)

directions <- directions_raw %>% 
  set_names(~tolower(sub(' Direction', '', .x))) %>% 
  slice(-1) %>% 
  separate(degree, c('degree_min', 'degree_max'), sep = '\\s+-\\s+', convert = TRUE)

wind_dir <- wind %>% 
  mutate(wd_cardinal = cut(
    windDirMean, 
    breaks = c(0, directions$degree_max, 360), 
    labels = c(directions$cardinal, 'N')
  ))
#we now have cardinal directions in NIWO_wind_dir#
str(wind_dir)
summary(wind_dir)

#onto mutating and binning windspeeds#

#convert windspeed from m/s to mph using mutate function and the conversion factor#
#add new variable windSpeedMPH#
wind_dir <- wind_dir %>%
  mutate(windSpeedMPH = windSpeedMean * 2.2369362920544) 

#choose breaks for binning windspeed values#
breaks <- c(0,10,20,30,40,50,75)

#group together measurements of chosen breaks using cut function#
wind_dir_binned <- wind_dir %>% 
  mutate(windspeed_bins = cut(windSpeedMPH, breaks = breaks))

view(wind_dir_binned)

#write over old file to make things easier#
aqua_wind <- wind_dir_binned

#omit NAs for proper plotting#
aqua_wind <- na.omit(aqua_wind)

##wind rose plot using ggplot##
# Using wind, plot wd filled by ws
ggplot(aqua_wind, aes(wd_cardinal, fill = windspeed_bins)) +
  # Add a bar layer with width 1
  geom_bar(width = 1)

# Convert to polar coordinates:
ggplot(aqua_wind, aes(wd_cardinal, fill = windspeed_bins)) +
  geom_bar(width = 1) +
  coord_polar()
# align the axis properly:
windrose_dirty <- ggplot(aqua_wind, aes(wd_cardinal, fill = windspeed_bins)) +
  geom_bar(width = 1) +
  coord_polar(start = -pi/16)
print(windrose_dirty)

#make it look nice#
windrose <- windrose_dirty +
  scale_fill_discrete(name = "Windspeed (MPH)", labels = c("0-10", "10-20", "20-30", "30-40", "40-50", "50 +")) +
  labs(x = "Wind Direction", y = "Number of Observations", size = 20) +
  ggtitle("5-Year Cumulative Windspeeds (30 min intervals)") +
  theme_minimal()
print(windrose)


###Separate and plot by site###
ARIK_aqua_wind <- subset(aqua_wind, aqua_wind$siteID == "ARIK")
COMO_aqua_wind <- subset(aqua_wind, aqua_wind$siteID == "COMO")
WLOU_aqua_wind <- subset(aqua_wind, aqua_wind$siteID == "WLOU")

#Plot ARIK Windrose                           
# Convert to polar coordinates:
ggplot(ARIK_aqua_wind, aes(wd_cardinal, fill = windspeed_bins)) +
  geom_bar(width = 1) +
  coord_polar()

# align the axis properly:
ARIK_windrose_dirty <- ggplot(ARIK_aqua_wind, aes(wd_cardinal, fill = windspeed_bins)) +
  geom_bar(width = 1) +
  coord_polar(start = -pi/16)
print(ARIK_windrose_dirty)

#make it look nice#
ARIK_windrose <- ARIK_windrose_dirty +
  scale_fill_discrete(name = "Windspeed (MPH)", labels = c("0-10", "10-20", "20-30", "30-40", "40-50", "50 +")) +
  labs(x = "Wind Direction", y = "Number of Observations", size = 20) +
  ggtitle("ARIK 7-Year Cumulative Windspeeds (30 min intervals)") +
  theme_minimal()
print(ARIK_windrose)

#Plot COMO Windrose                           
# Convert to polar coordinates:
ggplot(COMO_aqua_wind, aes(wd_cardinal, fill = windspeed_bins)) +
  geom_bar(width = 1) +
  coord_polar()

# align the axis properly:
COMO_windrose_dirty <- ggplot(COMO_aqua_wind, aes(wd_cardinal, fill = windspeed_bins)) +
  geom_bar(width = 1) +
  coord_polar(start = -pi/16)
print(COMO_windrose_dirty)

#make it look nice#
COMO_windrose <- COMO_windrose_dirty +
  scale_fill_discrete(name = "Windspeed (MPH)", labels = c("0-10", "10-20", "20-30", "30-40", "40-50", "50 +")) +
  labs(x = "Wind Direction", y = "Number of Observations", size = 20) +
  ggtitle("COMO 7-Year Cumulative Windspeeds (30 min intervals)") +
  theme_minimal()

#Plot WLOU Windrose                           
# Convert to polar coordinates:
ggplot(WLOU_aqua_wind, aes(wd_cardinal, fill = windspeed_bins)) +
  geom_bar(width = 1) +
  coord_polar()

# align the axis properly:
WLOU_windrose_dirty <- ggplot(WLOU_aqua_wind, aes(wd_cardinal, fill = windspeed_bins)) +
  geom_bar(width = 1) +
  coord_polar(start = -pi/16)
print(WLOU_windrose_dirty)

#make it look nice#
WLOU_windrose <- WLOU_windrose_dirty +
  scale_fill_discrete(name = "Windspeed (MPH)", labels = c("0-10", "10-20", "20-30", "30-40", "40-50", "50 +")) +
  labs(x = "Wind Direction", y = "Number of Observations", size = 20) +
  ggtitle("WLOU 7-Year Cumulative Windspeeds (30 min intervals)") +
  theme_minimal()

#y-axis log10 transformation#
ARIK_windrose + scale_y_log10()
COMO_windrose + scale_y_log10()
WLOU_windrose + scale_y_log10()
