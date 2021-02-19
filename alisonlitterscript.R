##Alright lets give this script writing thing a go##
##Script by Alison Haddad, largely copied from Ricky Lehrter, which was partially copied from WFH 44##
##February 17 2021



##Install or load packages, and set working directory. If you don't have a package, change 'library' to 'load.packages'
library(neonUtilities) ##download and stack data from the neon portal
library(tidyverse) ##includes dplyr and ggplot2
library(scales) ##for axis/graph manipulation

setwd("C:/Users/haddad/Documents/R/R-enthusiasts group/Alisons stuff")

#Alright, time to download that data
ltr <- loadByProduct(
  dpID = "DP1.10033.001",
  check.size = F, #remove the 'are yOu sURe YoU WaNt to DOwnLOad tHis DAta' message
  site = "RMNP",
  startdate = "2016-01" #date only worked for me in YYYY-MM format
)

View(ltr)

ltr.data <- ltr$ltr_massdata
View(ltr.data)

ltr.clean <-ltr.data %>% select(trapID, collectDate, functionalGroup, dryMass, qaDryMass) %>% drop_na()
View(ltr.clean)

#remove all the QA measurements (duplicate data)
ltr.semifinal <- filter(ltr.clean, qaDryMass!= "Y")
View(ltr.semifinal)

#remove the QA column now that its no longer useful
ltr.final <- ltr.semifinal %>% select(-qaDryMass) #can use -variable to just remove your chosen column
View(ltr.final)

##subset to just look at needle masses
ltr.needles <- filter(ltr.final, functionalGroup == "Needles")
View(ltr.needles)

ggplot(ltr.needles, aes(x= collectDate, y= dryMass))+
  geom_point()
  

##Want to average litter mass across traps
ltr.needles.avg <- ltr.needles %>% 
  group_by(trapID) %>% 
  mutate(meanDryMass = mean(dryMass)) #instead I switched to the mutate function, where I name to new column 
#meanDryMass on the left, and run the function mean() on the column dryMass on the right
#generally a bad idea to have () in your variable names if you can avoid it!
View(ltr.needles.avg)

#few basic plots
ggplot(ltr.needles.avg, aes(trapID, meanDryMass))+
  geom_point()

ggplot(ltr.needles.avg, aes(meanDryMass))+
  geom_boxplot()

##want to graph that but cant figure out how
##the column name is mean(dryMass), but when I try to use that, ggplot thinks I'm using the function 'mean' 
###and says that the variable "dryMass" doesn't exist. So I need to rename the column?? or somehow make ggplot 
####realize I dont wan't that function??


##subset to just have needles and leaves, and then plot 2 variables!
ltr.foliage <- filter(ltr.final, functionalGroup %in% c("Needles", "Leaves") )
View(ltr.foliage)

ggplot(ltr.foliage, aes(x = collectDate, y = dryMass))+
  geom_point(aes(color=factor(functionalGroup)))