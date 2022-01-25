library(neonUtilities)
library(tidyverse)
setwd('~/Git_R_group/R_Stacking_Workshop')
ALG_2016_2021 <- loadByProduct(dpID='DP1.20166.001', site=c("COMO","WLOU"), startdate="2016-01", enddate="2021-01", package="basic")
y
ALG_Tax<-ALG_2016_2021$alg_taxonomyProcessed
view(ALG_Tax)
ALG_Tax_COMO_WLOU <- ALG_Tax %>% select(siteID, sampleID, family, order, genus, scientificName)

ggplot(ALG_Tax_COMO_WLOU, aes(x = genus))+
  geom_bar(position = "dodge",aes(fill = siteID))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

count(ALG_Tax_COMO_WLOU,vars=scientificName)

ALG_Tax_COMO<-ALG_Tax_COMO_WLOU%>% filter(siteID=="COMO")
View(ALG_Tax_COMO)
ALG_Tax_WLOU<-ALG_Tax_COMO_WLOU%>% filter(siteID=="WLOU")
Counttable<-count(ALG_Tax_WLOU,vars=scientificName)
table(ALG_Tax_WLOU$scientificName)
write.csv(Counttable,file='counttable.csv')
