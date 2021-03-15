#######2019 Beetle Richness Mapping Project#########
####Learning vegan package####
#Vegan- great package for ecological data statistical analyses, especially useful for visualizing diversity

###Steps:
#1) Get NEON BET data for all sites in 2019, sorting records only
#2) Remove everything but site name, species name, date of collection, coordinates of trap (site would be better), abundance
#3) Calculate species richness as well as diversity indices using data
#4) Assign each site a set of coordinates and a richness value
#Final table- Site name, coordinates, species richness, diversity indices

library("ggplot2")
library("neonUtilities")
library("tidyr")
library("scales")
library("ggthemes")
library("dplyr")
library("vegan")

data("dune")

swc <- neonUtilities::loadByProduct(
  dpID="DP1.10022.001",
  check.size = F,
  site = "all",
  startdate = "2019-01",
  enddate = "2019-12"
)

str(dune)

#For vegan to work correctly and easily, we need our table in this format:
#Columns = Sites
#Rows = Observations
#Values = # of species collected at that site and date

bet <- swc$bet_parataxonomistID
str(bet)

bet_2 <- bet %>%
  select(siteID, domainID, collectDate, taxonID) %>%
             drop_na()
 
str(bet_2)

#exploring data#
n_obs <- bet_2 %>% count(siteID)

sp_count_persite <- bet_2 %>%
  count(siteID, taxonID) %>%
  arrange(siteID)

bet_alpharichness <- sp_count_persite %>%
  count(siteID)


###need lat and long###
#lets get them from the tower locations at each site#
View(swc$bet_fielddata)

towers <- neonUtilities::loadByProduct(
  dpID="DP1.00004.001",
  check.size = F,
  site = "all",
  startdate = "2019-11",
  enddate = "2019-12"
)

towers_cut <- towers$sensor_positions_00004
str(towers)

latlong <- towers_cut %>%
  select(siteID, referenceLatitude, referenceLongitude)

#join the tables to associate lat/long with richness and site data#
site_betrich_loc <- left_join(bet_alpharichness, latlong)

write.csv(site_betrich_loc,"C:/Users/rlehrter/Desktop/R Working Directory/neon_beetle_richness.csv", row.names = FALSE)