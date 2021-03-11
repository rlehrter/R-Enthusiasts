###GGPlot Learning

###Why GGPLOT? This is the best tool (package) in R for most people who are
###looking to make an attractive and meaningful plot.

###Also very user friendly once you get to know it

#Install/load your needed packages
library("ggplot2")
library("neonUtilities")
library("tidyr")
library("scales")
library("ggthemes")
library("dplyr")


###Get your data, this time for water chemistry as usual

#Tip- you can ?function() to get the proper syntax and arguments for that function.
?loadByProduct
  
#Load NEON data using neonUtilities
swc <- neonUtilities::loadByProduct(
  dpID="DP1.20093.001",
  check.size = F,
  site = c("ARIK", "COMO", "WLOU"),
  startdate = "2019-01",
  enddate = "2019-12"
)

#Select only the field data from the list of tables
aqua_field_data <- swc$swc_fieldSuperParent
View(aqua_field_data)

#drop unecessary variables, this time keep the siteID
aqua_final <- aqua_field_data %>%
  select(collectDate, waterTemp, dissolvedOxygen, specificConductance, siteID) %>%
  drop_na()

View(aqua_final)


###Time to plot. GGPLOT has 3 main components:
#Data - pretty obvious, you gotta tell R what data you want to use 
#Aesthetics - where R converts your data to visual scales
#Geometries - dictates what visual elements will be used on your plot

##Deconstructed basic ggplot, all three aqua sites date v. water temp
ggplot(
  aqua_final, #data- specify your dataframe from your global environment
  aes(x = collectDate, y = waterTemp))+ #aesthetics- tell R you want collectDate on X, water temp on Y
  geom_point() #geometry (aka "geom")- tell R that you want a scatterplot

#How could we make this more informative?

ggplot(
  aqua_final,
  aes(x = collectDate, y = waterTemp, shape = siteID))+
  geom_point()

#Not a great way to visualize this data, but it's an option!

ggplot(
  aqua_final,
  aes(x = collectDate, y = waterTemp, color = siteID))+
  geom_point()

#R automatically assigns colors to your variables

#Other geometries can display information in other ways
#geom_smooth() will draw a line of best fit using the "loess" method (non-parametric) as the default

ggplot(
  aqua_final,
  aes(x = collectDate, y = waterTemp))+
  geom_smooth(
)
#This is all three D10/13 sites combined, but we want them separated

ggplot(
  aqua_final,
  aes(x = collectDate, y = waterTemp, color = siteID))+
  geom_smooth(
)

#Can we make the gray confidence intervals a little less ugly? Sure
ggplot(
  aqua_final,
  aes(x = collectDate, y = waterTemp, color = siteID))+
  geom_smooth(aes(fill = siteID)
  )

#Why not add the points?
ggplot(
  aqua_final,
  aes(x = collectDate, y = waterTemp, color = siteID))+
  geom_point()+
  geom_smooth(aes(fill = siteID))

#And then add a theme
ggplot(
  aqua_final,
  aes(x = collectDate, y = waterTemp, color = siteID))+
  geom_point()+
  geom_smooth(aes(fill = siteID))+
  theme_tufte()

#See if you can improve this plot. Here's a few resources:
#Quick Reference to GGPLOT2- https://rstudio.com/wp-content/uploads/2015/03/ggplot2-cheatsheet.pdf
#Modifying Axes and Text- http://www.sthda.com/english/wiki/ggplot2-axis-ticks-a-guide-to-customize-tick-marks-and-labels
#Full GGPLOT2 tutorial- http://r-statistics.co/Complete-Ggplot2-Tutorial-Part1-With-R-Code.html