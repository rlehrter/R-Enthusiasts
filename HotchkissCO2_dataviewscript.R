###Script for plotting out sensor data from Hotchkiss CO2 Sensors###

###GET THE DATA
#First navigate to data location on your PC here: C:\Campbellsci\LoggerNet
#Open the CR1000_Table1.dat (for S1) OR CR1000_2_Table2.dat (for S2) file
#Save as, changing the suffix to a .csv and save to your working directory

###LOAD THE DATA
CO2_data <- read.csv("C:/Users/rlehrter/Desktop/R Working Directory/CR1000_2_Table1.csv")

###WRANGLE THE DATA (tidy er up)
#Get rid of column names that don't make sense and view dataframe again to make sure it worked
names(CO2_data) <- NULL
View(CO2_data)

#Get rid of the two rows (2 and 3) that we don't need
CO2_data <- CO2_data[-c(2,3),]

names(CO2_data) <- CO2_data[1,]
CO2_data <- CO2_data[-1,]
CO2_data <- CO2_data[,-c(7,8)]
CO2_data$TIMESTAMP <- as.Date(CO2_data$TIMESTAMP)
CO2_data$GP_CO2Conc <- as.numeric(CO2_data$GP_CO2Conc)
str(CO2_data)
View(CO2_data)
#Looks good! Column names are where they are supposed to be, got rid of the silly stuff

###PLOT THE DATA
library(ggplot2)
library(scales)
library(dplyr)

CO2_data <- CO2_data %>% group_by(TIMESTAMP) %>% summarize(mean(GP_CO2Conc))
CO2_data$GP_CO2Conc <- CO2_data$`mean(GP_CO2Conc)`

ggplot(CO2_data, aes(x = TIMESTAMP, y = GP_CO2Conc))+
  geom_line(show.legend = FALSE)+
  geom_smooth(color="blue")+
  scale_x_date(date_breaks = "1 month", labels = date_format("%b"))