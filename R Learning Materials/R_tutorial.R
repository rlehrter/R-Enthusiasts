################################ R Intro ####################################
#Mostly by RJL
#Partially stolen from R group 44

##### Preface - Basic Basics #####
# control enter will run the line of code your cursor is on, or whatever script is highlighted
# you can also click run in the top right of this box
# denotes a comment, which causes R to ignore everything to the right of it

################### Part 1: R Basics #######################

########## (1) R Studio Layout ##########
# Script Window
# Console
# Environment
# Plots, packages, help window


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


########## (3) Data Classes and Structures ##########
#Important to know when working with NEON data and the API!

### Common Classes
# Character (abc)
# Numeric (123)
# Logical (TRUE, FALSE)
# Date and Time (called "POSIXct")

# R is relatively smart and automatically assigns a data class to imported values
# But it doesn't always get it right, or maybe you want to change class for a specific reason 
# Fortunately there are "as." functions to change data class

# Examine the structure using str() or just the class using class() of variable "a"
str(B)
class(B)

# as.[insert class here] will allow you to change data to a new class
# the assignment operator (<-) here is "writing over" the old variable "a" with the newly assigned value
b <- as.logical(B)

# look again at the structure or class of that variable
str(b)
class(b)

### Common Data Structures
#Vector - 1 dimension dataset, every element is the same data type
#Matrix- 2 dimensional vector, every element is the same data type
#Data Frame- resembles a spreadsheet, can contain multiple data types, most common data structure you will use in R

##### (4) Basic R Operators #####
# <- assignment operator
# == equals
# != does not equal
# : selects everything between two values
# $ subset (we'll use this with NEON API data soon)

5 == 5
5 != 5 # Logical expressions like these can and will be used within many functions!
1:10
c <- 1:100
c
str(c)
########################################################


##### Part 2: API and Basic Plotting Tutorial #####

# This will walk through all steps to create a basic plot in R using water chemistry data from the API in the package GGPLOT2
# You can change it to work with any NEON data product


############# (1) Install and Load Packages, Set Working Directory ################
# First load all needed packages to your library to make them "accessible" or install.packages() if needed
# BTW a packages is just a bundle of functions that have been developed for specific purposes

library("neonUtilities") #has functions to pull and manipulate NEON data, including functions for the API
#API - (Application Programming Interface) essentially how R "talks" to the data portal
library("tidyverse") #a collection of multiple packages that include most of the functions you will need including:
#library("dplyr") #fantastic data manipulation tool
#library("ggplot2") #a much better alternative to the base R plotting/graphing functions
library("scales") #package I like for manipulating axes, breaks, labels and legends

# Determine the folder to which you want to save files, your working directory
# Right click folder, copy address as text, put "" around it, and change all \ to /
setwd("C:/Users/rlehrter/Desktop/R Working Directory")


############# (2) Get Your Awesome NEON Data Using the API ################
# Choose the site for which you want to determine ranges by assigning the name to variable "site"
# Must have "" around the text, otherwise R assumes this 4 letter code is a variable in the environment
site <- "ARIK"

# You will use the 'loadByProduct' function in the neonUtilities package to download all the water chemistry super parent data for the site
# You will not specify a date range, rather pull all the data for a site throughout all time to determine historic ranges
swc <- neonUtilities::loadByProduct(
  dpID="DP1.20093.001",# The data product ID for surface water chemistry. this can be changed to any data product ID
  check.size = F,# This input will force the function to download the data no matter the size of the download
  site = site, # This is the site you specified above
  startdate = "2019-01",
  enddate = "2019-12"
)

# Notice a new variable has appeared in the environment
# Let's look at it
str(swc)
# WELL

# This is what NEON data looks like in R before you subset it, or view it a different way (note the capital V)
View(swc)
#How to subset and view data
View(swc$swc_fieldSuperParent)
View(swc$swc_fieldSuperParent$waterTemp)
# So, NEON data comes as a list of 8 different tables, 7 of which we won't need.


############# (3) Manipulate and Organize Your Data ##############
# After looking at the contents of each it looks like We'll need only one of these, the swc_fieldSuperParent
# Let's subset swc to only what we need using the operator $
ARIK_field_data <- swc$swc_fieldSuperParent
str(ARIK_field_data)
View(ARIK_field_data)

###########################################################
#### Alternatively you can just use this "for loop" :) ####
#### GREAT CHUNK OF CODE FOR UNPACKING ENTIRE LIST ####
# Unpack data frames to work with data tables individually
for(i in 1:length(swc)) {
  assign(names(swc)[i], swc[[i]])
}
###########################################################

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
  labs(title = "ARIK Water Temperature", subtitle = "2019-2020", x = "Date", y = "Water Temperature")+
  scale_x_date(date_breaks = "1 month", labels = date_format("%b"))
#hot summer, cold winter

#Plot month vs dissolved oxygen
ggplot(ARIK_final, aes(x = new_dates, y = dissolvedOxygen))+
  geom_point()+
  geom_smooth()+
  labs(title = "ARIK Dissolved Oxygen", subtitle = "2019-2020", x = "Date", y = "Dissolved Oxygen")+
  scale_x_date(date_breaks = "1 month", labels = date_format("%b"))
#mirrors stream metabolism- hottest months, highest stream metabolism, lowest stream O2

#Plot month vs specific conductance
ggplot(ARIK_final, aes(x = new_dates, y = specificConductance))+
  geom_point()+
  geom_smooth()+
  labs(title = "ARIK Specific Conductance", subtitle = "2019-2020", x = "Date", y = "Specific Conductance")+
  scale_x_date(date_breaks = "1 month", labels = date_format("%b"))
#pretty consistent pattern with conductance- slightly higher in hot winter months where decreased water levels concentrate solutes

#That's it! Try changing the data product ID in the API and creating your own plot using different data