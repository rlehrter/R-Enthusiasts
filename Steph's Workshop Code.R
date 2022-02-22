###Load Libraries and Prepare Workspace

library("tidyverse")
library("neonUtilities")
library("vegan")

# clean out workspace

#rm(list = ls()) # OPTIONAL - clear out your environment
#gc()            # Uncomment these lines if desired

# load libraries 
library(tidyverse)

## -- Attaching packages --------------------------------------- tidyverse 1.3.0 --

## v ggplot2 3.3.5     v purrr   0.3.4
## v tibble  3.0.3     v dplyr   1.0.1
## v tidyr   1.1.1     v stringr 1.4.0
## v readr   1.3.1     v forcats 0.5.0

## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
## x dplyr::filter() masks stats::filter()
## x dplyr::lag()    masks stats::lag()

library(neonUtilities)
library(vegan)

## Loading required package: permute

## Loading required package: lattice

## This is vegan 2.5-7

# source .r file with my NEON_TOKEN
# source("my_neon_token.R") # OPTIONAL - load NEON token
# See: https://www.neonscience.org/neon-api-tokens-tutorial

###############################
###Download NEON Macroinvertebrate Data
# Macroinvert dpid
my_dpid <- 'DP1.20120.001'

# list of sites
my_site_list <- c('ARIK', 'POSE', 'MAYF')

# get all tables for these sites from the API -- takes < 1 minute
all_tabs_inv <- neonUtilities::loadByProduct(
  dpID = my_dpid,
  site = my_site_list,
  #token = NEON_TOKEN, #Uncomment to use your token
  check.size = F)
##############################
###Macroinvertebrate Data Munging
# what tables do you get with macroinvertebrate 
# data product
names(all_tabs_inv)

## [1] "categoricalCodes_20120" "inv_fieldData"          "inv_persample"          "inv_taxonomyProcessed"  "readme_20120"          
## [6] "validation_20120"       "variables_20120"

# extract items from list and put in R env. 
all_tabs_inv %>% list2env(.GlobalEnv)

## <environment: R_GlobalEnv>

# readme has the same informaiton as what you 
# will find on the landing page on the data portal

# The variables file describes each field in 
# the returned data tables
View(variables_20120)

# The validation file provides the rules that 
# constrain data upon ingest into the NEON database:
View(validation_20120)

# the categoricalCodes file provides controlled 
# lists used in the data
View(categoricalCodes_20120)

################################
###Reorganizing NEON Data
# known problem with dupes published in the inv_fieldData table as of 2021-02-18
# this anticipated to be fixed in data release next year (Jan 2022)
# use sampleID as primary key, keep the first uid associated with any sampleID that has multiple uids
de_duped_uids <- inv_fieldData %>% 
  group_by(sampleID) %>%
  summarise(n_recs = length(uid),
            n_unique_uids = length(unique(uid)),
            uid_to_keep = dplyr::first(uid)) 

# filter data using de-duped uids
inv_fieldData <- inv_fieldData %>%
  dplyr::filter(uid %in% de_duped_uids$uid_to_keep)




# extract year from date, add it as a new column
inv_fieldData <- inv_fieldData %>%
  mutate(
    year = collectDate %>% 
      lubridate::as_date() %>% 
      lubridate::year())




# extract location data into a separate table
table_location <- inv_fieldData %>%
  
  # keep only the columns listed below
  select(siteID, 
         domainID,
         namedLocation, 
         decimalLatitude, 
         decimalLongitude, 
         elevation) %>%
  
  # keep rows with unique combinations of values, 
  # i.e., no duplicate records
  distinct()




# create a taxon table, which describes each 
# taxonID that appears in the data set
# start with inv_taxonomyProcessed
table_taxon <- inv_taxonomyProcessed %>%
  
  # keep only the coluns listed below
  select(acceptedTaxonID, taxonRank, scientificName,
         order, family, genus, 
         identificationQualifier,
         identificationReferences) %>%
  
  # remove rows with duplicate information
  distinct()



# taxon table information for all taxa in 
# our database can be downloaded here:
# takes 1-2 minutes
# full_taxon_table_from_api <- neonUtilities::getTaxonTable("MACROINVERTEBRATE", token = NEON_TOKEN)




# Make the observation table.
# start with inv_taxonomyProcessed

# check for repeated taxa within a sampleID that need to be added together
inv_taxonomyProcessed_summed <- inv_taxonomyProcessed %>% 
  select(sampleID,
         acceptedTaxonID,
         individualCount,
         estimatedTotalCount) %>%
  group_by(sampleID, acceptedTaxonID) %>%
  summarize(
    across(c(individualCount, estimatedTotalCount), ~sum(.x, na.rm = TRUE)))




# join summed taxon counts back with sample and field data
table_observation <- inv_taxonomyProcessed_summed %>%
  
  # Join relevant sample info back in by sampleID
  left_join(inv_taxonomyProcessed %>% 
              select(sampleID,
                     domainID,
                     siteID,
                     namedLocation,
                     collectDate,
                     acceptedTaxonID,
                     order, family, genus, 
                     scientificName,
                     taxonRank) %>%
              distinct()) %>%
  
  # Join the columns selected above with two 
  # columns from inv_fieldData (the two columns 
  # are sampleID and benthicArea)
  left_join(inv_fieldData %>% 
              select(sampleID, eventID, year, 
                     habitatType, samplerType,
                     benthicArea)) %>%
  
  # some new columns called 'variable_name', 
  # 'value', and 'unit', and assign values for 
  # all rows in the table.
  # variable_name and unit are both assigned the 
  # same text strint for all rows. 
  mutate(inv_dens = estimatedTotalCount / benthicArea,
         inv_dens_unit = 'count per square meter')





# check for duplicate records, should return a table with 0 rows
table_observation %>% 
  group_by(sampleID, acceptedTaxonID) %>% 
  summarize(n_obs = length(sampleID)) %>%
  filter(n_obs > 1)

## # A tibble: 0 x 3
## # Groups:   sampleID [0]
## # ... with 3 variables: sampleID <chr>, acceptedTaxonID <chr>, n_obs <int>

# extract sample info
table_sample_info <- table_observation %>%
  select(sampleID, domainID, siteID, namedLocation, 
         collectDate, eventID, year, 
         habitatType, samplerType, benthicArea, 
         inv_dens_unit) %>%
  distinct()




# remove singletons and doubletons
# create an occurrence summary table
taxa_occurrence_summary <- table_observation %>%
  select(sampleID, acceptedTaxonID) %>%
  distinct() %>%
  group_by(acceptedTaxonID) %>%
  summarize(occurrences = n())





# filter out taxa that are only observed 1 or 2 times
taxa_list_cleaned <- taxa_occurrence_summary %>%
  filter(occurrences > 2)





# filter observation table based on taxon list above
table_observation_cleaned <- table_observation %>%
  filter(acceptedTaxonID %in%
           taxa_list_cleaned$acceptedTaxonID,
         !sampleID %in% c("MAYF.20190729.CORE.1",
                          "MAYF.20200713.CORE.1",
                          "POSE.20160718.HESS.1")) 
#this is an outlier sampleID






# some summary data
sampling_effort_summary <- table_sample_info %>%
  
  # group by siteID, year
  group_by(siteID, year, samplerType) %>%
  
  # count samples and habitat types within each event
  summarise(
    event_count = eventID %>% unique() %>% length(),
    sample_count = sampleID %>% unique() %>% length(),
    habitat_count = habitatType %>% 
      unique() %>% length())




# check out the summary table
sampling_effort_summary %>% as.data.frame() %>% 
  head() %>% print()

##   siteID year     samplerType event_count sample_count habitat_count
## 1   ARIK 2014            core           2            6             1
## 2   ARIK 2014 modifiedKicknet           2           10             1
## 3   ARIK 2015            core           3           11             2
## 4   ARIK 2015 modifiedKicknet           3           13             2
## 5   ARIK 2016            core           3            9             1
## 6   ARIK 2016 modifiedKicknet           3           15             1