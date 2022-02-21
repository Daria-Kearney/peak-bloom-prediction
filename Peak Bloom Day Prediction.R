####################################################################################################################
####################################################################################################################
####################################################################################################################
####################################################################################################################
####################################################################################################################
####################################################################################################################
#Daria's Data Set
#Creating data set for Cold and Warm Buckets- Daria
library(dplyr)
library(rnoaa)
library(tidyverse)
stations <- ghcnd_stations()

#Getting tmax data for 1950-2020
get_tmax<- function (stationid) {
  ghcnd_search(stationid = stationid, var = c("tmax"), 
               date_min = "1950-01-01", date_max = "2020-12-31")[[1]]}%>%
  mutate(year = as.integer(format(date, "%Y"))) %>%
  group_by(date) %>%
  summarize(tmax)

#Get all four location for 1950-2020
days_max<-
  tibble(location = "washingtondc", get_tmax("USC00186350")) %>%
  bind_rows(tibble(location = "liestal", get_tmax("GME00127786"))) %>%
  bind_rows(tibble(location = "kyoto", get_tmax("JA000047759"))) %>%
  bind_rows(tibble(location = "vancouver", get_tmax("CA001108395")))

#Getting tmin data for 1950-2020
get_tmin<- function (stationid) {
  ghcnd_search(stationid = stationid, var = c("tmin"), 
               date_min = "1950-01-01", date_max = "2020-12-31")[[1]]}%>%
  mutate(year = as.integer(format(date, "%Y"))) %>%
  group_by(date) %>%
  summarize(tmin) 

#Get all four location for 1950-2020
days_min<-
  tibble(location = "washingtondc", get_tmin("USC00186350")) %>%
  bind_rows(tibble(location = "liestal", get_tmin("GME00127786"))) %>%
  bind_rows(tibble(location = "kyoto", get_tmin("JA000047759"))) %>%
  bind_rows(tibble(location = "vancouver", get_tmin("CA001108395")))

#Merging temp max and min for all four locations
tempall<- merge(days_max, days_min, by=c("location", "date"))

#Getting temperature average
tempall$tavg <- rowMeans(tempall[ , c("tmax", "tmin")], na.rm= FALSE)

#Creating binary variable label "cold" for when average temperature is <= -115 (1/10 °C)=> 11°F
tempall$cold<- ifelse(tempall$tavg <= -115, 1, 0)

#Creating binary variable label "warm" for when average temperature is <= 100(1/10 °C)=> 50°F
tempall$hot<- ifelse(tempall$tavg >= 100, 1, 0)

#Getting the data for just Nov, Dec, Jan, & Feb
temp_all_hot_cold<-subset(tempall, format(date, "%m") %in% c("01", "02", "11", "12"))

#Creating new variable of the sum of days that meet the requirement above & 
#grouping them in Spring & Winter so I can aggregate the date
temp_all_hot_cold<- temp_all_hot_cold %>%
  mutate(month = as.integer(format(date, "%m"))) %>%
  mutate(year = as.integer(format(date, "%Y"))) %>%
  mutate(seasons= ifelse(month < 11, "Spring", "Winter")) %>%
  group_by(year, seasons, location) %>%
  mutate(cold= sum(cold, na.rm = TRUE)) %>%
  mutate(hot= sum(hot, na.rm = TRUE)) %>%
  mutate(cold = ifelse(seasons == "Spring", is.na("cold"), cold)) %>%
  mutate(hot = ifelse(seasons == "Winter", is.na("hot"), hot))

#selecting important data
temp_all_hot_cold<- select(temp_all_hot_cold, "location", "year", "cold", "hot")

#Grouping data by Year location and season
temp_all_hot_cold<- temp_all_hot_cold %>%
  group_by(year) %>%
  group_by(location, year, seasons) %>% summarize_all(list(~toString(unique(.))))

#Erasing season variable
temp_all_hot_cold<- subset(temp_all_hot_cold, select = -c(seasons))

#Transforming cold and hot to numeric so I can aggregate
temp_all_hot_cold<-transform(temp_all_hot_cold, cold= as.numeric(cold), hot= as.numeric(hot))
str(temp_all_hot_cold)

#Aggregating data so that it is by location and have just one row for each year for all locations
temp_all_hot_cold= aggregate(. ~ year + location, data = temp_all_hot_cold, FUN= sum)

#Might have to change the criteria for the average temperature.
#Still seeing what is the best model and predictions

#Merging bloom days for the three locations note: takes away Vancouver days
cherry <- read.csv("data/washingtondc.csv") %>% 
  bind_rows(read.csv("data/liestal.csv")) %>% 
  bind_rows(read.csv("data/kyoto.csv"))

cherrytembin<- merge(cherry, temp_all_hot_cold, by= c("location", "year"))

#Getting rid of unnecessary variables
cherrytembin<- select(cherrytembin, -c("alt", "lat", "long", "bloom_date"))

#Fitting linear regression to see how it performs
test_ls_fit<- lm(bloom_doy ~ year + location + cold + hot, data = cherrytembin)
summary(test_ls_fit)

#RSS
sum(resid(test_ls_fit)^2)
deviance(test_ls_fit)
#MSE
mean(test_ls_fit$residuals^2)
#AIC
AIC(test_ls_fit)
#All better than standard LR given in demo

#Fitting it for the past predictions for 3 locations
cherrytembin<- cherrytembin %>% 
  bind_cols(predicted_doy = predict(test_ls_fit, newdata= cherrytembin))

# Plot the predictions alongside the actual observations for 1950 up to 2020.
cherrytembin %>% 
  ggplot(aes(x = year, y = predicted_doy)) +
  geom_line() +
  geom_point(aes(y = bloom_doy)) +
  facet_grid(cols = vars(str_to_title(location))) +
  labs(x = "Year", y = "Peak bloom (days since Jan 1st)")

#Calculating Absolute Difference for three locations between 1950-2020 getting the sums for 2011-2020
diff<- cherrytembin %>%
  group_by(year, location) %>%
  mutate(absdiff = abs(predicted_doy-bloom_doy)) %>%
  #filter(year>= 2011) %>% #
  group_by(year) %>%
  filter(year>=2011) %>%
  summarise(sum= sum(absdiff))
print(diff)
#Calculating the mean of the sum of the Absolute Difference for three locations between 1950-2020
#The mean over the 70 years is 15.5 which performs better than demo
mean(diff$sum)
sum(diff$sum)
####################################################################################################################
####################################################################################################################
####################################################################################################################
####################################################################################################################
####################################################################################################################
####################################################################################################################