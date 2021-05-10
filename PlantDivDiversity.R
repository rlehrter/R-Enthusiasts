###Shannon's and Simpson's Diversity for Plant Div Data using Vegan###

library("ggplot2")
library("neonUtilities")
library("tidyr")
library("scales")
library("ggthemes")
library("dplyr")
library("vegan")


###CPER###
cperdiv <- neonUtilities::loadByProduct(
  dpID="DP1.10058.001",
  check.size = F,
  site = "CPER",
  startdate = "2014-01",
  enddate = "2019-12"
)

cperplants <- cperdiv$div_1m2Data

str(cperplants)

###cut down to important variables (columns)
cperplantsclean <- cperplants %>%
  select(siteID, plotID, endDate, taxonID, scientificName, family) %>%
  drop_na()

###exploratory plot
ggplot(cperplantsclean, aes(x=family)) +
  geom_bar() +
  theme_minimal()

###transform data table to a format usable by vegan (diversity calculator package)
cperabundance <- cperplantsclean %>%
  group_by(plotID, family) %>%
  tally() %>%
  spread(family, n, fill = 0) %>%
  ungroup()

###make a table of just cperplot IDs
###temporarily remove plotIDs to make vegan happy
###ungroup() is CRUCIAL to get this to work
cperplots <- cperabundance %>%
  select(plotID)

cperabundance2 <- cperabundance %>%
  select(-plotID)

###calculate Shannon's and Simpson's diversity indices###
cpershannons <- data.frame(diversity(cperabundance2, index = "shannon"))
cpersimpsons <- data.frame(diversity(cperabundance2, index = "simpson"))

###Add "cperplots" plotID values to "cpershannons" and "cpersimpsons", make one table
###rename cperplot_divindices
cperplot_divindices <- cbind(cperplots, cpershannons, cpersimpsons)

###rename columns to something sensible
cperplot_divindices <- cperplot_divindices %>%
  rename(
    shannons_diversity = diversity.cperabundance2.,
    simpsons_diversity = diversity.cperabundance2..index....simpson..
  )

###unfortunately all the land cover types are the same by plot! Are there any other good
###explanatory variables for your response variable (plant diversity)?