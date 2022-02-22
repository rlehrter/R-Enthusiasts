#This plot looks at CPER soil CO2 data from all plots at all depths from 2016-2021
#More work still needed on looking for bad data/working with outliers?

#load all libraries
library(ggplot2)
library(tidyverse)
library(rvest)
library(neonUtilities)
library(dplyr)
library(lubridate)

#get all data from the portal with the selected time frame
soilco2 <- loadByProduct(dpID='DP1.00095.001', site = "CPER", package = "basic")

#view data
View(soilco2$SCO2C_30_minute)

#subset for variables needed
CPER_soilco2 <- (soilco2$'SCO2C_30_minute'[c("siteID", "startDateTime", "horizontalPosition", "verticalPosition", "soilCO2concentrationMean")])

#change Depth to a factor variable for the plot
CPER_soilco2$verticalPosition <- as.factor(CPER_soilco2$verticalPosition)

#make a modified copy of the original data to change facet wrap labels later
CPER_soilco2_mod <- CPER_soilco2 %>%
mutate(horizontalPosition = recode(horizontalPosition, "001" = "Plot 1", "002" = "Plot 2", "003" = "Plot 3", "004" = "Plot 4", "005" = "Plot 5"))


#boxplot
ggplot(CPER_soilco2_mod, aes(x=verticalPosition, y=soilCO2concentrationMean, fill=verticalPosition)) +
  geom_boxplot()+
  scale_y_continuous(limits=c(350,550))+
  facet_wrap(~horizontalPosition, nrow = 1, ncol = 5)+
  ggtitle("CPER Soil Plot CO2 Distribution", subtitle = "2016-2021")+
  ylab("Average CO2 Concentration (ppm)")+
  scale_fill_discrete(labels=c('Shallow', 'Medium', 'Deep'))+
  labs(fill='Depth')+
  theme(axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank())
  
  