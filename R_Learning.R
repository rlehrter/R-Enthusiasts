################################ R Intro ####################################
#Mostly by RJL
#Partially stolen from R group 44


################### Part 1: R Basics #######################

########## (1) R Studio Layout ##########
# Script Window
# Console
# Environment
# Plots, packages, help window

# Btw, a # denotes a comment, which causes R to ignore everything to the right of it
# Use it to make notes
#########################################


########## (2) Basic Operations ##########
# R is basically a fancy calculator
12+5
40/8

# You can also assign numbers to letters, or "objects"
a <- 1
B <- 12
# Note these are stored in your environment window
# Also, R doesn't allow spaces in assigned names and IS caps sensitive

#Now you can do math with them
B-a
#########################################


##### (3) Data Classes and Structures #####
#Important to know when working with NEON data and the API!

### Common Classes
# Character (abc)
# Numeric (123)
# Logical (TRUE, FALSE)
# Date and Time (POSIXct)

# R is relatively smart and automatically assigns a data class to imported values
# But it doesn't always get it right, or maybe you want to change class for a specific reason 

##### Fortunately there are functions to change data class #####
# Examine the structure using str() or just the class using class() of variable "a"
str(a)
class(a)

# as.[insert class here] will allow you to change data to a new class
# the assignment operator (<-) here is "writing over" the old variable "a" with the newly assigned value
a <- as.logical(a)

# look again at the structure or class of that variable
str(a)
class(a)

### Common Data Structures
#Vector - 1 dimension dataset, every element is the same data type
#Matrix- 2 dimensional vector, every element is the same data type
#Data Frame- resembles a spreadsheet, can contain multiple data types, most common data structure you will use in R

##### (4) Basic R Operators #####
# <- assignment operator
# == <- equals
# != <- does not equal
# : <- selects everything between two values
# $ <- subset (we'll use this with NEON API data soon)

5 == 5
5 != 5
1:10
c <- 1:100
c
str(c)

# These logical expressions can and will be used within many functions!


##### Part 2: API and Basic Plotting Tutorial #####

# This will walk through all steps to create a basic plot in R using water chemistry data from the API in the package GGPLOT2
# You can change it to work with any NEON data product


# First load all needed packages to your library to make them "accessible" or install.packages() if needed
# BTW a packages is just a bundle of functions that have been developed for specific purposes

library("neonUtilities")
library("dplyr")
library("tidyverse")
library("ggplot2")
library("scales")

# neonUtilities has functions to pull and manipulate NEON data, specifically
# dplyr is a fantastic data manipulation tool
# ggplot2 is a much better alternative to the base R plotting/graphing functions

# Determine the folder to which you want to save files, your working directory
# Right click folder, copy address as text, put "" around it, and change all \ to /
setwd("C:/Users/rlehrter/Desktop/R Working Directory")

### Now onto getting data! ###
# Choose the site for which you want to determine ranges by assigning the name to variable "site"
# Must have "" around the text, otherwise R assumes this 4 letter code is a variable in the environment
site <- "ARIK"

# You will use the 'loadByProduct' function in the neonUtilities package to download all the water chemistry super parent data for the site
# You will not specify a date range, rather pull all the data for a site throughout all time to determine historic ranges
swc <- neonUtilities::loadByProduct(
  dpID="DP1.20093.001",# The data product ID for surface water chemistry. this can be changed to any data product ID
  check.size = F,# This input will force the function to download the data no matter the size of the download
  site = site# This is the site you specified above
)

# Notice a new variable has appeared in the environment
# Let's look at it
str(swc)
# WELL

# This is what NEON data looks like in R before you subset it, or view it a different way (note the capital V)
View(swc)
# So, NEON data comes as a list of 8 different tables, 7 of which we won't need.

# After looking at the contents of each it looks like We'll need only one of these, the swc_fieldSuperParent
# Let's subset swc to only what we need using the operator $
ARIK_field_data <- swc$swc_fieldSuperParent
str(ARIK_field_data)
View(ARIK_field_data)

###################################
#### Alternatively you can just use this "for loop" :) ####
#### GREAT CHUNK OF CODE FOR UNPACKING ENTIRE LIST ####
# Unpack data frames to work with data tables individually
for(i in 1:length(swc)) {
  assign(names(swc)[i], swc[[i]])
}
###################################

# Let's make the table more manageable by removing all variables we don't need, then all observations (rows) that have a missing value, or "NA" 
ARIK_field_data_cut <- ARIK_field_data %>%
  select(collectDate, waterTemp, dissolvedOxygen, specificConductance) %>%
  drop_na()

#Temporary workaround- Doing date things for plotting purposes here to trick R into thinking these observations are all from 1 year
new_dates <- format(ARIK_field_data_cut$collectDate, format = "%m-%d")
new_dates <- as.Date(new_dates, format = "%m-%d")
ARIK_final <- data.frame(ARIK_field_data_cut, new_dates)


#Plot 5 year cumulative water ARIK water temp on Y vs. month on X
ggplot(ARIK_final, aes(x = new_dates, y = waterTemp))+
  geom_point()+
  geom_smooth()+
  labs(title = "ARIK Water Temperature", subtitle = "5 Years of Data", x = "Date", y = "Water Temperature")+
  scale_x_date(date_breaks = "1 month", labels = date_format("%b"))
#hot summer, cold winter

#Plot month vs dissolved oxygen
ggplot(ARIK_final, aes(x = new_dates, y = dissolvedOxygen))+
  geom_point()+
  geom_smooth()+
  labs(title = "ARIK Dissolved Oxygen", subtitle = "5 Years of Data", x = "Date", y = "Dissolved Oxygen")+
  scale_x_date(date_breaks = "1 month", labels = date_format("%b"))
#mirrors stream metabolism- hottest months, highest stream metabolism, lowest stream O2

#Plot month vs specific conductance
ggplot(ARIK_final, aes(x = new_dates, y = specificConductance))+
  geom_point()+
  geom_smooth()+
  labs(title = "ARIK Specific Conductance", subtitle = "5 Years of Data", x = "Date", y = "Specific Conductance")+
  scale_x_date(date_breaks = "1 month", labels = date_format("%b"))
#pretty consistent pattern with conductance- slightly higher in hot winter months where decreased water levels concentrate solutes
