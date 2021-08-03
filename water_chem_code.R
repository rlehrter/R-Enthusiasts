###Code and help for making some sweet mid-season presentation plots###
##RJL 2021##

#first set your working directory to a desktop folder in "Session", "Set Working Directory"

#install all your needed packages
install.packages("tidyverse")
install.packages("dplyr")
install.packages("neonUtilities")
install.packages("ggplot2")
install.packages("scales")
install.packages("lubridate")

#load them
library(neonUtilities)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(scales)
library(lubridate)

#use NEON API to download ARIK swc data#
ARIK_swc <- neonUtilities::loadByProduct(
  dpID="DP1.20093.001",# The data product ID for surface water chemistry. this can be changed to any data product ID
  check.size = F,# This input will force the function to download the data no matter the size of the download
  site = "ARIK" # This is the site you specified above
)

#download COMO
COMO_swc <- neonUtilities::loadByProduct(
  dpID="DP1.20093.001",# The data product ID for surface water chemistry. this can be changed to any data product ID
  check.size = F,# This input will force the function to download the data no matter the size of the download
  site = "COMO" # This is the site you specified above
)

#download WLOU
WLOU_swc <- neonUtilities::loadByProduct(
  dpID="DP1.20093.001",# The data product ID for surface water chemistry. this can be changed to any data product ID
  check.size = F,# This input will force the function to download the data no matter the size of the download
  site = "WLOU" # This is the site you specified above
)

###start of data wrangling###
#subset to just select relevant table from each list
ARIK <- ARIK_swc$swc_fieldSuperParent
COMO <- COMO_swc$swc_fieldSuperParent
WLOU <- WLOU_swc$swc_fieldSuperParent

#manipulate ARIK dates to just be in a month-day format, no year
ARIK_new_dates <- as.Date(ARIK$collectDate, format = ("%m-%d"))
date <- format(ARIK_new_dates, format= ("%m-%d"))
ARIK_1 <- data.frame(ARIK, date)
ARIK_1$date <- paste0('2021-', ARIK_1$date)
ARIK_1$date <- as.Date.character(ARIK_1$date)

#choose only needed variables
ARIK_fin <- ARIK_1 %>%
  select(siteID, collectDate, date, waterTemp, dissolvedOxygen, specificConductance) %>%
  drop_na()

#manipulate COMO dates to just be in a month-day format, no year
COMO_new_dates <- as.Date(COMO$collectDate, format = ("%m-%d"))
date <- format(COMO_new_dates, format= ("%m-%d"))
COMO_1 <- data.frame(COMO, date)
COMO_1$date <- paste0('2021-', COMO_1$date)
COMO_1$date <- as.Date.character(COMO_1$date)

#choose only needed variables
COMO_fin <- COMO_1 %>%
  select(siteID, collectDate, date, waterTemp, dissolvedOxygen, specificConductance) %>%
  drop_na()

#manipulate WLOU dates to just be in a month-day format, no year
WLOU_new_dates <- as.Date(WLOU$collectDate, format = ("%m-%d"))
date <- format(WLOU_new_dates, format= ("%m-%d"))
WLOU_1 <- data.frame(WLOU, date)
WLOU_1$date <- paste0('2021-', WLOU_1$date)
WLOU_1$date <- as.Date.character(WLOU_1$date)

#choose only needed variables
WLOU_fin <- WLOU_1 %>%
  select(siteID, collectDate, date, waterTemp, dissolvedOxygen, specificConductance) %>%
  drop_na()

#combine all three data frames (all three sites)
D1013_vitals <- ARIK_fin %>% 
full_join(COMO_fin) %>% 
full_join(WLOU_fin)

###Okay, data all wrangled, let's plot###
#Water Temperature
ggplot(D1013_vitals, aes(x = date, y = waterTemp))+
  geom_point(aes(color = siteID), show.legend = FALSE)+
  geom_smooth(aes(fill = siteID), color = "black")+
  scale_fill_manual(values = c("green1", "steelblue1", "orange1"))+
  scale_color_manual(values = c("green4", "steelblue3", "orange3"))+
  labs(title = "D10/13 Water Temperatures", subtitle = "2012-2021", x = "Calendar Month", y = "Water Temperature (C)", fill = "Site Code")+
  scale_x_date(date_breaks = "1 month", labels = date_format("%b"))

#Dissolved Oxygen
ggplot(D1013_vitals, aes(x = date, y = dissolvedOxygen))+
  geom_point(aes(color = siteID), show.legend = FALSE)+
  geom_smooth(aes(fill = siteID), color = "black")+
  scale_fill_manual(values = c("green1", "steelblue1", "orange1"))+
  scale_color_manual(values = c("green4", "steelblue3", "orange3"))+
  labs(title = "D10/13 Dissolved Oxygen", subtitle = "2012-2021", x = "Calendar Month", y = "Dissolved Oxygen (mg/L)", fill = "Site Code")+
  scale_x_date(date_breaks = "1 month", labels = date_format("%b"))

#Specific Conductance
ggplot(D1013_vitals, aes(x = date, y = specificConductance))+
  geom_point(aes(color = siteID), show.legend = FALSE)+
  geom_smooth(aes(fill = siteID), color = "black")+
  scale_fill_manual(values = c("green1", "steelblue1", "orange1"))+
  scale_color_manual(values = c("green4", "steelblue3", "orange3"))+
  labs(title = "D10/13 Specific Conducance", subtitle = "2012-2021", x = "Calendar Month", y = "Specific Conductance (spc)", fill = "Site Code")+
  scale_x_date(date_breaks = "1 month", labels = date_format("%b"))