#Trying to figure out MOS date things for 
#plotting purposes to trick R into thinking observations are all from 1 year

library(neonUtilities)
setwd('~/GitHub/Mosquito.data/')

#To look at only our sites, need collect function c()
MOSQ_2016_2020<-loadByProduct(dpID='DP1.10043.001', site =c("NIWO","RMNP","CPER","STER"), startdate = "2016", enddate = "2021", package = "basic")
y
View(MOSQ_2016_2020)
View(MOSQ_2016_2020$mos_expertTaxonomistIDProcessed)

#####Removing the exporting as .csv and importing as this screwed up the data file#####
#write.csv(MOSQ_2016_2020$mos_expertTaxonomistIDProcessed, file = 'MOSQ_taxon_D10.13_2016_2020.csv')
#mosq_taxa<-read.csv("C:/Users/mccahill/Documents/GitHub/Mosquito.data/MOSQ_taxon_D10.13_2016_2020.csv")
#####

#####start here##################

mosq_taxa <- MOSQ_2016_2020$mos_expertTaxonomistIDProcessed

#load ggplot2 and dplyr packages
library(ggplot2)
library(dplyr)
library(tidyverse)
library(scales)


NIWO<-mosq_taxa %>% filter(siteID=="NIWO")
View(NIWO)

taxon <- mosq_taxa$scientificName
genus <- mosq_taxa$genus


#Screwing around with Ricky's date code 

#Temporary workaround- Doing date things for plotting purposes here to trick R into thinking these observations are all from 1 year

#Collectdate is factor need to convert to a POSIX (Just date not time)
posix_dates <- as.POSIXct.Date(NIWO$collectDate)
View(posix_dates)

new_dates <- as.Date(posix_dates, format = "%m-%d")
View(new_dates)

NIWO_final_date <- data.frame(NIWO, new_dates)
View(NIWO_final_date)


#TRYING AGAIN
new_Dates <- as.Date(NIWO$collectDate, format = ("%m-%d"))

NIWO_new_dates <- data.frame(NIWO, new_Dates)

View(NIWO_new_dates)


#only NIWO CollectDate vs number of mosq
ggplot(NIWO,aes(x=new_dates,y=individualCount, color = taxon))+
  geom_jitter()+
  coord_cartesian(ylim = c(0,150)) +
  scale_x_date(date_breaks = "1 month",labels = date_format("%b"))






