library(rnoaa)
library(tidyverse)
library(lubridate)
library(astsa)
library(dynlm)

options(scipen = 100)

cherry <- read.csv("data/washingtondc.csv") %>% 
  bind_rows(read.csv("data/liestal.csv")) %>% 
  bind_rows(read.csv("data/kyoto.csv"))

cherry %>% filter(year >= 1950) %>%
  ggplot(aes(year,bloom_doy, color=location)) +
  geom_line() +
  labs(x="Year",
       y="Days from January 1st",
       title="Peak Bloom Dates (1950-2021)") +
  theme_minimal()

## Estimating linear trend in all locations, prior to 2000 for forecasting

kyoto <- dplyr::filter(cherry,location=="kyoto",year<2000)
liestal <- dplyr::filter(cherry,location=="liestal",year<2000)
dc <- dplyr::filter(cherry,location=="washingtondc", year<2000)

kyoto_bloom <- ts(kyoto$bloom_doy,frequency=1)
liestal_bloom <- ts(liestal$bloom_doy,frequency=1)
dc_bloom <- ts(dc$bloom_doy,frequency=1)

# Trend component for modeling
trend_k <- time(kyoto_bloom)
trend_l <- time(liestal_bloom)
trend_dc <- time(dc_bloom)

summary(tfit_k <- lm(kyoto_bloom ~ trend_k))
summary(tfit_l <- lm(liestal_bloom ~ trend_l))
summary(tfit_dc <- lm(dc_bloom ~ trend_dc))

### All plots suggest lagged 1 variable is statistically significant 
plot(diff(kyoto_bloom), type="o")
mean(diff(kyoto_bloom)) # drift estimate = .002
acf(diff(dc_bloom), 48)

plot(diff(liestal_bloom), type="o")
mean(diff(liestal_bloom)) # drift estimate = -0.114
acf(diff(liestal_bloom), 48)

plot(diff(dc_bloom), type="o")
mean(diff(dc_bloom)) # drift estimate = .205
acf(diff(dc_bloom), 48)

## Scatterplot on the lagged variables shows no influence of non-linear relationships
lag1.plot(kyoto_bloom,4)
lag1.plot(liestal_bloom,4)
lag1.plot(dc_bloom, 4)



