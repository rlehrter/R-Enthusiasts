library(neonUtilities)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(scales)
library(wesanderson)
?wesanderson
stackByTable(filepath="C:/Users/rlehrter/Desktop/Water Chem Data Fun/NEON_discharge-stream.zip")

D1013_vitals <- read.csv("C:/Users/rlehrter/Desktop/Water Chem Data Fun/swc_fieldSuperParent_mod.csv")

tail(D1013_vitals)

D1013_vitals

#separates just the site into it's own vector... not necessarily helpful
site_vector <- D1013_vitals$siteID
ARIK_only[order(ARIK_only$Date),]

str(D1013_vitals)
str(D1013_acid)

#Reads dates into new data frame called new_dates. "Tricks" ggplot into adding them all to the same mm-dd by adding 2020
#to each date, even though observations are from multiple years. Custom format cells in excel to "mm-dd" to get this to work.
new_dates <- as.Date(D1013_vitals$Date, format = "%m-%d")
D1013_vitals_newdates <- data.frame(D1013_vitals, new_dates)
head(D1013_vitals_newdates)

#finally, able to subset by one site in the siteID column and include all variables
ARIK_only <- subset(D1013_vitals_newdates, siteID == "ARIK")

COMO_only <- subset(D1013_vitals_newdates, siteID == "COMO")

WLOU_only <- subset(D1013_vitals_newdates, siteID == "WLOU")

#attempting to plot water temp for each site
ggplot(ARIK_only, aes(x = new_dates, y = waterTemp))+
  geom_point()+
  geom_smooth()+
  labs(title = "ARIK Water Temperature", subtitle = "5 Years of Data", x = "Calendar Month", y = "Water Temperature")+
  scale_x_date(date_breaks = "1 month", labels = date_format("%b"))

ggplot(COMO_only, aes(x = new_dates, y = waterTemp))+
  geom_point()+
  geom_smooth()+
  labs(title = "COMO Water Temperature", subtitle = "5 Years of Data", x = "Calendar Month", y = "Water Temperature")+
  scale_x_date(date_breaks = "1 month", labels = date_format("%b"))

ggplot(WLOU_only, aes(x = new_dates, y = waterTemp))+
  geom_point()+
  geom_smooth()+
  labs(title = "WLOU Water Temperature", subtitle = "3 Years of Data", x = "Calendar Month", y = "Water Temperature")+
  scale_x_date(date_breaks = "1 month", labels = date_format("%b"))



#On one plot- oh yeah!
ggplot(D1013_vitals, aes(x = new_dates, y = waterTemp))+
  geom_point(aes(color = siteID), show.legend = FALSE)+
  geom_smooth(aes(fill = siteID), color = "black")+
  scale_fill_manual(values = c("green1", "steelblue1", "orange1"))+
  scale_color_manual(values = c("green4", "steelblue3", "orange3"))+
  labs(title = "D10/13 Water Temperatures", subtitle = "2015-2020", x = "Calendar Month", y = "Water Temperature (°C)", fill = "Site Code")+
  scale_x_date(date_breaks = "1 month", labels = date_format("%b"))

ggplot(D1013_vitals, aes(x = new_dates, y = dissolvedOxygen))+
  geom_point(aes(color = siteID), show.legend = FALSE)+
  geom_smooth(aes(fill = siteID), color = "black")+
  scale_fill_manual(values = c("green1", "steelblue1", "orange1"))+
  scale_color_manual(values = c("green4", "steelblue3", "orange3"))+
  labs(title = "D10/13 Dissolved Oxygen", subtitle = "2015-2020", x = "Calendar Month", y = "Dissolved Oxygen (mg/L)", fill = "Site Code")+
  scale_x_date(date_breaks = "1 month", labels = date_format("%b"))

ggplot(D1013_vitals, aes(x = new_dates, y = specificConductance))+
  geom_point(aes(color = siteID), show.legend = FALSE)+
  geom_smooth(aes(fill = siteID), color = "black")+
  scale_fill_manual(values = c("green1", "steelblue1", "orange1"))+
  scale_color_manual(values = c("green4", "steelblue3", "orange3"))+
  labs(title = "D10/13 Specific Conductance", subtitle = "2015-2020", x = "Calendar Month", y = "Specific Conductance (??S/cm)", fill = "Site Code")+
  scale_x_date(date_breaks = "1 month", labels = date_format("%b"))
  


###GREAT!###
#Attempting to plot ALK for all three sites with facet wrap
D1013_acid <- read.csv("C:/Users/rlehrter/Desktop/Water Chem Data Fun/swc_domainLabData_mod.csv")
head(D1013_acid)

##altering dates to have the same year so they can be plotted in one "calendar year"
new_dates2 <- as.Date(D1013_acid$Date, format = "%m-%d")
D1013_acid_newdates <- data.frame(D1013_acid, new_dates2)
head(D1013_acid_newdates)

acid <- ggplot(D1013_acid, aes(x = new_dates2, y = alkMgPerL)) +
  geom_point(aes(color = siteID), show.legend = FALSE) +
  geom_smooth(aes(fill = siteID), color = "black", alpha = 0.4) +
  scale_fill_manual(values = c("green1", "steelblue1", "orange1"))+
  scale_color_manual(values = c("green4", "steelblue3", "orange3"))+
  labs(title = "D10/13 Alkalinity", subtitle = "2015-2020", x = "Calendar Month", y = "Alkalinity mg/L CaCO3", fill = "siteID")+
  scale_x_date(date_breaks = "1 month", labels = date_format("%b"))

acid + facet_grid(rows = vars(siteID), scales = "free_y")

ancid <- ggplot(D1013_acid, aes(x = new_dates2, y = ancMgPerL)) +
  geom_point(aes(color = siteID), show.legend = FALSE) +
  geom_smooth(aes(fill = siteID), color = "black", alpha = 0.4) +
  scale_fill_manual(values = c("green1", "steelblue1", "orange1"))+
  scale_color_manual(values = c("green4", "steelblue3", "orange3"))+
  labs(title = "D10/13 Acid Neutralizing Capacity", subtitle = "2015-2020", x = "Calendar Month", y = "mg/L DOC", fill = "siteID")+
  scale_x_date(date_breaks = "1 month", labels = date_format("%b"))

ancid + facet_grid(rows = vars(siteID), scales = "free_y")


