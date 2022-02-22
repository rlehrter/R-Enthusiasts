#smams join#
library(dplyr)


mam <- neonUtilities::loadByProduct(
  dpID="DP1.10072.001",
  check.size = F,
  site = "STER",
  startdate = "2018-01"
)

mam_path <- neonUtilities::loadByProduct(
  dpID="DP1.10064.001",
  check.size = F,
  site = "STER",
  startdate = "2018-01"
)

mam2 <- mam$mam_pertrapnight
  
mam_path2 <- mam_path$rpt_bloodtesting

newdf <- mam2 %>% inner_join(mam_path2, by = "bloodSampleID")

View(newdf)
