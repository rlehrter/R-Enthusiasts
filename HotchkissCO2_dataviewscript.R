###Script for plotting out sensor data from Hotchkiss CO2 Sensors###
###RJLII

###GET THE DATA
#First navigate to data location on your PC here: C:\Campbellsci\LoggerNet
#Open the CR1000_Table1.dat (for S1) OR CR1000_2_Table2.dat (for S2) file
#Save as, changing the suffix to a .csv and save to your working directory

library(lubridate)

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

#New function, this turned out awesome! Get that lubridate cheat sheet
CO2_data$DateHour <- floor_date(as.POSIXct(CO2_data$TIMESTAMP), unit = "hour") 

CO2_data$DateHour <- as.POSIXct(CO2_data$DateHour)
CO2_data$GP_CO2Conc <- as.numeric(CO2_data$GP_CO2Conc)
str(CO2_data)
View(CO2_data)
#Looks good! Column names are where they are supposed to be, got rid of the silly stuff

###PLOT THE DATA
#But first summarize in a meaningful way (averaged over each hour in this case)
CO2_data_2 <- CO2_data %>%
  group_by(DateHour) %>%
  summarize(mean(GP_CO2Conc)) %>%
  ungroup()

#Rename the column just created or R will get very confused
CO2_data_2$GP_CO2Conc <- CO2_data_2$`mean(GP_CO2Conc)`

#Plot with extra lines for a fun glow effect
ggplot(CO2_data_2, aes(x = DateHour, y = GP_CO2Conc))+
  geom_line(color="dodgerblue1", size = 2)+
  geom_line(color="dodgerblue2", size = 1.5)+
  geom_line(color="dodgerblue3", size = 1)+
  geom_smooth(color= "orange")+
  theme_minimal()+
  labs(x = "", y = "CO2 Concentration (ppm)", title = "CO2 Concentration at COMO S2 (Hotchkiss AA)")