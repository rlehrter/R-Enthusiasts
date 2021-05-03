###Kelley's Mammal Aging Project Overview:###

#Thanks Ricky Lehrter and Nanodegree Programming with R class for
#help with all this code!!!

#Finalizing Mammal Project 2021. Looking at PEMA and ONLE species first
#(species with blatant pelage change across life stages) to see if there is
#a relationship between age and weight, then applying to the rest of common species at D10/13.
#Generate weight parameter tables for each species to use in the field.

##Tip: Ctrl+shift+C will block out chunk of text with "##"

#Load necessary packages
library(neonUtilities)
library(ggplot2)
library(dplyr)
library(tidyr)
library(tidyverse) #Alternatively, only tidyverse will load both dplyr and tidyr packages
library(rstatix)#newer 2021 packages, for anova_test to compare mean of multiple groups using ANOVA test
library(ggpubr)#also for anova test

#Set working directory - data will be saved into .csv and saved here
setwd('~/Mammal_Aging_Project/MAM_Data_Poster/')

#Download D10.13 mammal data off the portal to include mammal data from our 4 sites 
#from 2016 - 2020 then save locally. Start and end date format "YYYY-MM-DD"
MAM_2016_2021<-loadByProduct(dpID='DP1.10072.001', site =c("NIWO","RMNP","CPER","STER"), startdate = "2016", enddate = "2021", package = "basic")
y

###Simply exclude start/end dates, if wanted to look at ALL data (all sites) up until 2020 (2013 - 2020)
###NAME<-loadByProduct(dpID='DP1.10072.001', site ="all", package = "basic")

#Check data to ensure includes appropriate sampling years (2016 - 2020). View function will open up separate window with dataset 
View(MAM_2016_2021$mam_pertrapnight)

#save .csv locally. Make sure name has no "/" in it (D10.13 not D10/13)
write.csv(MAM_2016_2021$mam_pertrapnight, file = 'MAM_D10.13_2016_2020.csv')

#.csv saved locally on your comp, do not need to download/stack from portal again. Start here from now on!

######################################

#Set working directory and load packages if not done so yet

#Read data from the csv into R. 
mam_d10.13<-read.csv("C:\\Users\\mccahill\\Documents\\Mammal_Aging_Project\\MAM_Data_Poster\\MAM_D10.13_2016_2020.csv")

View(mam_d10.13)#RMNP has only 2017 - 2020 data since it was established in 2016.

#Let's look at PEMAs first. Filter dataset to reflect just PEMA taxon and captures
#Use the pipe (%>%) (tidyverse package) to filter
#Vertical bar (|) means and/or, use to select multiple values within column. Use & to include another column value within filter.
#can switch out PEMA to any other taxa!!
pema<-mam_d10.13 %>% filter((trapStatus=="5 - capture"|trapStatus=="4 - more than 1 capture in one trap")& taxonID=="PEMA")

#Clean up dataset to only these columns and remove NAs #maybe include elevation, nlcdClass, repo status in future?
pema_na.rm <- pema %>% select(lifeStage, weight, taxonID, siteID, collectDate) %>% drop_na()

View(pema_na.rm) #ensure all NAs removed

#NA stands for 'Not Applicable'. This is how R handles missing values

#######
#what is R calling each variable?
sapply(pema_na.rm, class)

#Some basics, nice to know!
View(pema_na.rm) #view entire dataset 
names(pema_na.rm) #see column titles of datafile
head(pema_na.rm) #returns data in first 6 rows 
dim(pema_na.rm) #index information, 2091 rows of 5 columns
summary(pema_na.rm) #summary stats of each column
######

#Before I plot, I know lifestage is not ordered from J to S to A (instead S J A). 
#Need to rearrange order of lifestage on x axis and convert to factor to do so (if needed?)
lifeStage<-factor(pema_na.rm$lifeStage, levels = c("juvenile","subadult","adult"))


#Use ggplot2 function to create visualizations. Use + to add multiple layers within ggplot. 
#Really cool thing is you can build up your plots in layers!!!

#aes function to map which variables to compare.
#if only want to look at multiple species make new filter (see chipmunks below)

#y axis is weight (continuous variable) and x will be lifeStage (categorical variable)
ggplot(pema_na.rm, aes(x=lifeStage, y=weight, fill = lifeStage))+ 
  #quickly see difference between distributions of variables (median, max/min, interquartile ranges) 
  geom_boxplot()+ 
  #get rid of "unknown" value for lifestage
  scale_x_discrete(limit = c("juvenile","subadult","adult"))+ 
  #facet wrap separates into different graphs per site. 
  #creates same type of plot for each level of your categorical variable (additional notes below)
  facet_wrap(vars(siteID))+ 
  #add limits to the plot to zoom in on bulk of distribution, adjust axes to get a better look at data
  #Use coord_cartesian to limit y axis without removing data points from calculations, all data included in calcs.
  #This ensures stat output matches boxplots (quantile values match plot)
  #Using y lim parameter or scale_y_continuous layer will actually remove datapoints
  coord_cartesian(ylim = c(0,35))+ 
  #Add labels in one layer
  labs(title = "North American Deer Mouse Age vs. Weight (D10/13)", subtitle = "Raw Data from 2016 - 2020 (RMNP 2017-2020)", x = "Life Stage", y = "Weight")+
  #used fill to color boxplot, so removed legend due to redundancy
  theme(legend.position = "none") 

#interquartile range within the box - represents middle 50% of values in our samples (where they fall, between Q1 and Q3). Outlier data points, not admitting any data in this plot even though zoomed in
#median - middle value separating the higher half from the lower half of dataset
#1st quartile - 25% of adult RMNP PEMAs weigh more than 14 grams
#3rd quartile - 75% of adult RMNP PEMAs weigh below 18 grams (25% weigh more than 18 g)
##outliers are just outside max/min, 1.5 x the IQR from the median


##Displays sample size (n), mean, max, min, and interquartile range (Q1, median, Q3) for all lifestages at each site.
#Define as new dataset to view this table in separate window
pema_stats <- pema_na.rm %>% 
  group_by(lifeStage, siteID) %>% 
  summarise(
    n = n(),
    mean = mean(weight),
    max = max(weight),
    min = min(weight),
    quantile(weight)
  )

#Wooo check it!!! 
View(pema_stats)


##Some more helpful & simple stats##
table(pema_na.rm$lifeStage) #see how many (number of) PEMA J,S,A there are

by() #by command, understand distribution of the data compare to interquartile range
#Average weight by lifestage - takes 3 inputs, variable, a categorical variable to subset over, and a function
#So we summarize weight by lifestage for all (regardless of site)
by(pema_na.rm$weight, pema_na.rm$lifeStage, summary)#variable, categorical variable, summ stat


##########################
##Now for all chipmunks

#Filter data to reflect just chipmunk taxa and captures
chipmunks<-mam_d10.13 %>% filter((trapStatus=="5 - capture"|trapStatus=="4 - more than 1 capture in one trap")& taxonID=="TAMI"|taxonID=="TAQU"|taxonID=="TAUM")
View(chipmunks)

#Clean up to only these columns
chipmunks_na.rm <- chipmunks %>% select(lifeStage, weight, taxonID, siteID, collectDate) %>% drop_na()

View(chipmunks_na.rm)

#Color code by taxon using fill
ggplot(chipmunks_na.rm, aes(x=lifeStage, y=weight, fill=taxonID))+
  geom_boxplot()+
  scale_x_discrete(limit = c("juvenile","subadult","adult"))+ 
  facet_wrap(vars(siteID))+
  coord_cartesian(ylim = c(0,70))+
  xlab("Life Stage")+ylab("Weight")+
  labs(title = "Chipmunk Age vs. Weight (D10/13)") 

#We never catch juvenile chipmunks - don't leave nest until eyes open (subadult)

chipmunk_stats <- chipmunks_na.rm %>% 
  group_by(lifeStage, siteID) %>% 
  summarise(
    n = n(),
    mean = mean(weight),
    max = max(weight),
    min = min(weight),
    quantile(weight)
  )

View(chipmunk_stats)

#############################
#Now for ONLE Northern grasshopper mouse

onle<-mam_d10.13 %>% filter((trapStatus=="5 - capture"|trapStatus=="4 - more than 1 capture in one trap")& taxonID=="ONLE")

onle_na.rm <- onle %>% select(lifeStage, weight, taxonID, siteID) %>% drop_na()
View(onle_na.rm)

ggplot(onle_na.rm, aes(x=lifeStage,y=weight, fill = lifeStage))+
  geom_boxplot()+
  scale_x_discrete(limit = c("juvenile","subadult","adult"))+ 
  facet_wrap(vars(siteID))+
  coord_cartesian(ylim = c(0,65))+
  xlab("Life Stage")+
  ylab("Weight")+
  labs(title = "Northern Grasshopper Mouse Age to Weight (D10/13)") 

onle_stats <- onle_na.rm %>% 
  group_by(lifeStage, siteID) %>% 
  summarise(
    n = n(),
    mean = mean(weight),
    max = max(weight),
    min = min(weight)
  )

View(onle_stats)

###########################
#CHHI or Hispid pocket mouse

chhi<-mam_d10.13 %>% filter((trapStatus=="5 - capture"|trapStatus=="4 - more than 1 capture in one trap")& taxonID=="CHHI")

chhi_na.rm <- chhi %>% select(lifeStage, weight, taxonID, siteID) %>% drop_na()
View(chhi_na.rm)

ggplot(chhi_na.rm, aes(x=lifeStage,y=weight, fill = lifeStage))+
  geom_boxplot()+
  scale_x_discrete(limit = c("juvenile","subadult","adult"))+ 
  facet_wrap(vars(siteID))+
  coord_cartesian(ylim = c(0,80))+
  xlab("Life Stage")+
  ylab("Weight")+
  labs(title = "Hispid Pocket Mouse Age to Weight (D10/13)") 

chhi_stats <- chhi_na.rm %>% 
  group_by(lifeStage, siteID) %>% 
  summarise(
    n = n(),
    mean = mean(weight),
    max = max(weight),
    min = min(weight),
    quantile(weight)
  )

View(chhi_stats)
##########################


##Results:##
#Weight parameter tables for each mammal species & using in the field during the 2021 season!
#Will also be measuring body length this season to get 2-factor parameter for body mass


###Future Directions:###
#Figure out how to display weight parameters in text boxes on graph
#Look at PEMA vs elevation
#Create map of mam species richness across observatory
#Use GLMMs to include other factors influencing relationship b/w age vs. weight (NLCD class, elevation, repo status) 




##### ANOVA test run on PEMA ####

??anova_test

#going to try comparing the mean of multiple groups using ANOVA test
pema.anova <- pema_na.rm %>% anova_test(weight ~ lifeStage)

pema.anova

#Ok now do a pairwise T-tests for multiple groups - compares mean weight of 3 life stages to each other
pwc <- pema_na.rm %>%
  pairwise_t_test(weight ~ lifeStage, p.adjust.method = "bonferroni")

pwc

#not sure how to interpret this just yet but NOOT significant whatsoever I don't think?

#put values on the boxplot graph***



######################################
#Additional Notes:#

###Facet_wrap layer:###
#add a layer called facet wrap(). A tilde and variable to split our data over
#facet_wrap takes on a formula inside of it's parenthenses 
#facet_wrap( formula )
#formula is a tilde sign followed by name of the variable you want to facet over
#facet_wrap(~ variable )

#same function but set number of column it displays to 3 (3 graphs side by side)
#facet_wrap(vars(siteID, ncol = 3))


####Facet_grid layer:###
#facet_grid is better for faceting over two or more variables (if only using one varaible use facet_wrap)
#facet_grid also takes formula but in different form
#contains variables want to split over in vertical direction followed by ~ sign
#then name of variable want to split in horizontal direction 

#where you place variable can change how graph is laid out and orientation of them 
#facet_grid ( vertical ~ horizontal )
#For example: 
  #facet_grid(dob_month~gender) #each row is a different month, each column a different gender

#Adding period/dot in forumla for facet_grid() represents all of the other variables in the data set
#This notation splits up the data by gender and produces 3 histograms, each having their own row
#for example: facet_grid(gender ~ .)

###Faceting summary:###
#facet_wrap(~variable_to_split_on)
#facet_grid(rows_variable~columns_variable)"

####################################


###Omitting NA values Cont'd###
#use subset command and use condition as the second parameter (first parameter just dataset)
#Instead of using all of the data we can remove the NA values from the dataset associated with gender 
#ggplot(aes(x = weight), data = subset(pema, !is.na(sex))) + geom_histogram()
#If only wanted to remove NAs from one column We could also use is.na function with a bang (exclamation mark)
#This removes any rows that have NAs for gender variable

#Another way to subset data - is using na.omit() function, remove any observations (rows) 
#that have NA in them not necessarily just for gender.
##AKA the na.omit wrapper removes any rows (or mammals) that have NA for ANY of the variables in the data set##

