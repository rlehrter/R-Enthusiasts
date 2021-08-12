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
ggsave("water_temp.pdf", width = 10, height = 8)

#Dissolved Oxygen
ggplot(D1013_vitals, aes(x = date, y = dissolvedOxygen))+
  geom_point(aes(color = siteID), show.legend = FALSE)+
  geom_smooth(aes(fill = siteID), color = "black")+
  scale_fill_manual(values = c("green1", "steelblue1", "orange1"))+
  scale_color_manual(values = c("green4", "steelblue3", "orange3"))+
  labs(title = "D10/13 Dissolved Oxygen", subtitle = "2012-2021", x = "Calendar Month", y = "Dissolved Oxygen (mg/L)", fill = "Site Code")+
  scale_x_date(date_breaks = "1 month", labels = date_format("%b"))
ggsave("dissolved_oxygen.pdf", width = 10, height = 8)

#Specific Conductance
ggplot(D1013_vitals, aes(x = date, y = specificConductance))+
  geom_point(aes(color = siteID), show.legend = FALSE)+
  geom_smooth(aes(fill = siteID), color = "black")+
  scale_fill_manual(values = c("green1", "steelblue1", "orange1"))+
  scale_color_manual(values = c("green4", "steelblue3", "orange3"))+
  labs(title = "D10/13 Specific Conducance", subtitle = "2012-2021", x = "Calendar Month", y = "Specific Conductance (spc)", fill = "Site Code")+
  scale_x_date(date_breaks = "1 month", labels = date_format("%b"))
ggsave("conductance.pdf", width = 10, height = 8)

###Plots have been exported to your working directory using ggsave(). Check it out!

###Let's try and wrangle then plot alk
d1013acid <- ARIK_swc$swc_domainLabData %>%
  full_join(COMO_swc$swc_domainLabData) %>%
  full_join(WLOU_swc$swc_domainLabData)

head(d1013anc)

#manipulate dates as before to just be in a month-day format, no year
d1013acid_new_dates <- as.Date(d1013acid$collectDate, format = ("%m-%d"))
date <- format(d1013anc_new_dates, format = ("%m-%d"))
d1013acid_fin <- data.frame(d1013acid, date)
d1013acid_fin$date <- paste0('2021-', d1013acid_fin$date)
d1013acid_fin$date <- as.Date.character(d1013acid_fin$date)

#choose only needed variables
d1013alk <- d1013acid_fin %>%
  select(siteID, collectDate, date, alkMgPerL) %>%
  drop_na()

#End wrangling, start plotting
ggplot(d1013alk, aes(x = date, y = alkMgPerL))+
  geom_point(aes(color = siteID), show.legend = FALSE)+
  geom_smooth(aes(fill = siteID), color = "black")+
  scale_fill_manual(values = c("green1", "steelblue1", "orange1"))+
  scale_color_manual(values = c("green4", "steelblue3", "orange3"))+
  labs(title = "D10/13 Alkalinity", subtitle = "2012-2021", x = "Calendar Month", y = "Alkalinity (mg/L)", fill = "Site Code")+
  scale_x_date(date_breaks = "1 month", labels = date_format("%b"))

#Oof, looks like we have some outliers (data integrity issue). Will need to remove these.
View(d1013alk)
#Sorted on the alk column to see outliers, removed by row.
d1013alk_clean <- d1013alk[-c(34, 385, 388),] 

#Let's try plotting again
ggplot(d1013alk_clean, aes(x = date, y = alkMgPerL))+
  geom_point(aes(color = siteID), show.legend = FALSE)+
  geom_smooth(aes(fill = siteID), color = "black")+
  scale_fill_manual(values = c("green1", "steelblue1", "orange1"))+
  scale_color_manual(values = c("green4", "steelblue3", "orange3"))+
  labs(title = "D10/13 Alkalinity", subtitle = "2012-2021", x = "Calendar Month", y = "Alkalinity (mg/L)", fill = "Site Code")+
  scale_x_date(date_breaks = "1 month", labels = date_format("%b"))
#Looks great! Export this plot.
ggsave("alkalinity.pdf", width = 10, height = 8)

#####
###Now how about cell counts?
d1013_cc <- neonUtilities::loadByProduct(
  dpID="DP1.20138.001",# The data product ID for surface water chemistry. this can be changed to any data product ID
  check.size = F,# This input will force the function to download the data no matter the size of the download
  site = c("ARIK", "COMO", "WLOU")# This is the site you specified above
)

d1013cc <- d1013_cc$amc_cellCounts

#manipulate dates as before to just be in a month-day format, no year
d1013cc_new_dates <- as.Date(d1013cc$collectDate, format = ("%m-%d"))
date <- format(d1013cc_new_dates, format = ("%m-%d"))
d1013cc_fin <- data.frame(d1013cc, date)
d1013cc_fin$date <- paste0('2021-', d1013cc_fin$date)
d1013cc_fin$date <- as.Date.character(d1013cc_fin$date)

#choose only needed variables
d1013cc <- d1013cc_fin %>%
  select(siteID, collectDate, date, rawMicrobialAbundance) %>%
  drop_na()

ggplot(d1013cc, aes(x = date, y = rawMicrobialAbundance))+
  geom_point(aes(color = siteID), show.legend = FALSE)+
  geom_smooth(aes(fill = siteID), color = "black")+
  scale_fill_manual(values = c("green1", "steelblue1", "orange1"))+
  scale_color_manual(values = c("green4", "steelblue3", "orange3"))+
  labs(title = "D10/13 Cell Counts", subtitle = "2012-2021", x = "Calendar Month", y = "Raw Microbial Abundance (n)", fill = "Site Code")+
  scale_x_date(date_breaks = "1 month", labels = date_format("%b"))
###What a let down, this data is worthless.. I probably wouldn't show this plot. 
#Not enough observations to make for a robust plot, note the large confidence intervals.
#####


###S1 and S2 sensor data? Turbidity, chlorophyll, pH. BIG DATA, be patient with R.
d1013_wq <- neonUtilities::loadByProduct(
  dpID="DP1.20288.001",# The data product ID for surface water chemistry. this can be changed to any data product ID
  check.size = F,# This input will force the function to download the data no matter the size of the download
  site = c("ARIK", "COMO", "WLOU"),
  startdate = "2018-01"
  )

#subset list to only the dataset we want
d1013wq <- d1013_wq$waq_instantaneous

#want to keep variables of interest and QF indicators for filtering
d1013wq2 <- d1013wq %>%
  select(siteID, endDateTime, pH, pHFinalQF, turbidity, turbidityFinalQF, chlorophyll, chlorophyllFinalQF) %>%
  drop_na()

#subset to remove any observations with quality flagged data, which = "no good"
d1013wq_QF_rm <- subset(d1013wq2, d1013wq2$pHFinalQF == 0 & d1013wq2$turbidityFinalQF == 0 & d1013wq2$chlorophyllFinalQF == 0)

d1013wq_new_dates <- as.Date(d1013wq_QF_rm$endDateTime, format = ("%m-%d"))
date <- format(d1013wq_new_dates, format = ("%m-%d"))
d1013wq_fin <- data.frame(d1013wq_QF_rm, date)
d1013wq_fin$date <- paste0('2021-', d1013wq_fin$date)
d1013wq_fin$date <- as.Date.character(d1013wq_fin$date)

#randomly subset the dataset to only 1000 observations so that R can process it and so the plot doesn't look so crazy
d1013wq_sub <- sample_n(d1013wq_fin, 10000)

#This plot is awesome in conjunction with the ALK data. More consistent pH at ARIK shows how it is "more resistant"
#to changes in pH because of it's higher alkalinity, and thus higher buffering capacity against changes in pH. 
#On the other hand, COMO has a huge amount of variation in pH. WLOU isn't quite as consistent as ARIK either.
ggplot(d1013wq_sub, aes(x = date, y = pH))+
  geom_point(aes(color = siteID), show.legend = FALSE)+
  geom_smooth(aes(fill = siteID), color = "black")+
  scale_fill_manual(values = c("green1", "steelblue1", "orange1"))+
  scale_color_manual(values = c("green4", "steelblue3", "orange3"))+
  labs(title = "D10/13 pH", subtitle = "2018-2021", x = "Calendar Month", y = "pH", fill = "Site Code")+
  scale_x_date(date_breaks = "1 month", labels = date_format("%b"))
ggsave("pH.pdf", width = 10, height = 8)

#Next one plotted on a log scale to account for huge chlorophyll measurements. 
#Note that ARIK is pretty consistent throughout the year, while COMO and WLOU drop down...
#to nearly 0 during winter months (almost 0 primary production when everything is frozen)
ggplot(d1013wq_sub, aes(x = date, y = chlorophyll))+
  geom_point(aes(color = siteID), show.legend = FALSE)+
  geom_smooth(aes(fill = siteID), color = "black")+
  scale_fill_manual(values = c("green1", "steelblue1", "orange1"))+
  scale_color_manual(values = c("green4", "steelblue3", "orange3"))+
  labs(title = "D10/13 Chlorophyll", subtitle = "2018-2021", x = "Calendar Month", y = "Chlorophyll (ug/L)", fill = "Site Code")+
  scale_x_date(date_breaks = "1 month", labels = date_format("%b"))+
  scale_y_log10()
ggsave("chlorophyll.pdf", width = 10, height = 8)

###This next plot just looks like a shotgun blast... got a data quality issue here, I think. I would not show.
ggplot(d1013wq_sub, aes(x = date, y = turbidity))+
  geom_point(aes(color = siteID), show.legend = FALSE)+
  geom_smooth(aes(fill = siteID), color = "black")+
  scale_fill_manual(values = c("green1", "steelblue1", "orange1"))+
  scale_color_manual(values = c("green4", "steelblue3", "orange3"))+
  labs(title = "D10/13 Turbidity", subtitle = "2018-2021", x = "Calendar Month", y = "Turbidity", fill = "Site Code")+
  scale_x_date(date_breaks = "1 month", labels = date_format("%b"))+
  scale_y_log10()
ggsave("turbidity.pdf", width = 10, height = 8)
