library(rnoaa)
library(tidyverse)
library(lubridate)
library(astsa)

cherry <- read.csv("data/washingtondc.csv") %>% 
  bind_rows(read.csv("data/liestal.csv")) %>% 
  bind_rows(read.csv("data/kyoto.csv"))

dc <- read.csv("data/washingtondc.csv")
liestal <- read.csv("data/liestal.csv")
kyoto <- read.csv("data/kyoto.csv")

cherry %>% filter(year >= 1950) %>%
          ggplot(aes(year,bloom_doy)) +
             geom_line() +
             facet_wrap(~location)

(r = round(acf(dc$bloom_doy, 8, plot=FALSE)$acf[-1], 8))
plot(stats::lag(dc$bloom_doy,-8), dc$bloom_doy); legend('topleft', legend=r[8])

head(tempall)
tempall<- merge(days_max, days_min, by=c("location", "date"))
tempall$tavg <- rowMeans(tempall[ , c("tmax", "tmin")], na.rm= FALSE)
tempall <- mutate(tempall,
                  year = as.integer(format(tempall$date,"%Y")))

year_temp <- tempall %>%
  group_by(year,location) %>%
  summarise(mean_temp = mean(tavg, na.rm=TRUE))

dc_temp <- dplyr::filter(year_temp, location=="washingtondc")
kyoto_temp <- dplyr::filter(year_temp, location=="kyoto")
liestal_temp <- dplyr::filter(year_temp, location=="liestal")

acf(dc_temp$mean_temp)
acf(kyoto_temp$mean_temp)
acf(liestal_temp$mean_temp)
