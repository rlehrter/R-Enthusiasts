library(dplyr)
library(ggplot2)
library(lubridate)

fish <- neonUtilities::loadByProduct(
  dpID="DP1.20107.001",
  check.size = F,
  site = "WLOU",
  startdate = "2018-01"
)

fish_cut <- fish$fsh_perFish %>% select("passStartTime", "taxonID", "fishTotalLength", "fishWeight")
fish_cut$date <- as.Date(fish_cut$passStartTime)
fish_cut$time <- format(fish_cut$passStartTime,"%H:%M:%S")
fish_cut$number <- (1:398)
split_dates$number <- (1:398)

split_dates <- data.frame(date = fish_cut$date,
                 year = as.numeric(format(fish_cut$date, format = "%Y")),
                 month = as.numeric(format(fish_cut$date, format = "%m")),
                 day = as.numeric(format(fish_cut$date, format = "%d")))

df_new <- inner_join(fish_cut, split_dates, by = "number")

df_new2 <- df_new %>% group_by(year, month) %>% count(taxonID)

View(df_new2)
                    