library(neonUtilities)
library(scales)
library(lubridate)
library(ggplot2)
library(plotly)

temps <- neonUtilities::loadByProduct(
  dpID="DP1.00002.001",
  check.size = F,
  site = c("ARIK","COMO","WLOU","NIWO","CPER","STER","RMNP"),
  startdate = "2020-01",
  enddate = "2021-01"
)

temps_df <- temps$SAAT_30min
str(temps_df)
 
temps_df2 <- temps_df %>% select(siteID, startDateTime, tempSingleMean)
str(temps_df2)

temps_df2$Month <- format(as.Date(temps_df2$startDateTime), "%m")

ggplot(temps_df2, aes(x = Month, y = tempSingleMean, fill = siteID)) +
  geom_boxplot()