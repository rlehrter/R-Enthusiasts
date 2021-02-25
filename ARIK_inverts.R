library("neonUtilities")
library("dplyr")
library("tidyverse")
library("ggplot2")
library("scales")

setwd("C:/Users/hschartel/Desktop/R Working Directory")

site <- "ARIK"

invert <- ltr <- loadByProduct(
  dpID = "DP1.20120.001",
  check.size = F, #remove the 'are yOu sURe YoU WaNt to DOwnLOad tHis DAta' message
  site = site,
  startdate = "2016-01" #date only worked for me in YYYY-MM format
)
names(invert)
View(invert$inv_taxonomyProcessed$collectDate)

View(invert)
ARIK_tax <- invert$inv_taxonomyProcessed
View(ARIK_tax)

#isolated the data I wanted to use
ARIK_tax_cut <- ARIK_tax %>%
  select(family, estimatedTotalCount) %>%
  drop_na()

boxplot(ARIK_tax_cut$family~ARIK_tax_cut$estimatedTotalCount, main="ARIK Taxonomy", 
        xlab="Family", ylab="estimatedTotalCount") #this one didn't work

#then I tried this ggplot(ARIK_tax_cut, aes(x=family, y=estimatedTotalCount)) + geom_bar(stat="identity") No color
#but hey, lets make it fancy. added the steel blue
#but it's still a mess
#I wanted to flip the x axis text so I could actually read it
graph <- ggplot(ARIK_tax_cut, aes(x=family, y=estimatedTotalCount)) + geom_bar(stat="identity", fill="steelblue")

graph + theme(axis.text.x = element_text(face = "bold", size = 12, angle = 90))

#ok I flipped it, but I still can't read it. I think I need to space them out more


