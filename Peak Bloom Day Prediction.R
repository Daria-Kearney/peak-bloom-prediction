##########################
# Load Packages and Data #
##########################

library(tidyverse)
library(mgcv) # gam function for splines
library(rnoaa)
library(geosphere) # distm function for euclidean distance
library(randomForest)
library(missForest) # missForest()
library(gbm) # gbm()
library(xgboost) # xgb.train() and xgboost()

# Load data
cherry <- read.csv("data/washingtondc.csv") %>% 
  bind_rows(read.csv("data/liestal.csv")) %>% 
  bind_rows(read.csv("data/kyoto.csv"))

########################################################
# RNOAA data wrangling and Avg Daily Sunlight (KJ/m^2) #
########################################################

stations <- ghcnd_stations()

stations[stations$id=="USC00186350",] # washington dc
stations[stations$id=="GME00127786",] # liestal
stations[stations$id=="JA000047759",] # kyoto
stations[stations$id=="CA001108395",] # vancouver

#Distance from Vancouver airport to maple grove park
distm(c(-123.1833, 49.2), c(-123.160242, 49.223586), fun = distHaversine) #lon, lat
#3115 meters

get_temperature <- function (stationid) {
  ghcnd_search(stationid = stationid, var = c("TMAX"), 
               date_min = "1950-01-01", date_max = "2022-01-31")[[1]] %>%
    mutate(year = as.integer(format(date, "%Y")),
           month = as.integer(strftime(date, '%m')) %% 12, # make December "0"
           season = cut(month, breaks = c(0, 2, 5, 8, 11),
                        include.lowest = TRUE,
                        labels = c("Winter", "Spring", "Summer", "Fall")),
           year = if_else(month == 0, year + 1L, year)) %>%
    group_by(year, season) %>%
    summarize(tmax_avg = mean(tmax, na.rm = TRUE))
}

historic_temperatures <-
  tibble(location = "washingtondc", get_temperature("USC00186350")) %>%
  bind_rows(tibble(location = "liestal", get_temperature("GME00127786"))) %>%
  bind_rows(tibble(location = "kyoto", get_temperature("JA000047759"))) %>%
  bind_rows(tibble(location = "vancouver", get_temperature("CA001108395")))

get_climatedata <- function (stationid) {
  historic_climatedata <- ghcnd_search(stationid = stationid, var = c("TMAX", "TMIN", "PRCP", "SNWD"), 
                                       date_min = "1850-01-01", date_max = "2023-01-31")
  
  multiFull <- merge(merge(merge(
    historic_climatedata[[1L]],
    historic_climatedata[[2L]][-c(4:6)], all=TRUE, by=c("id", "date")),
    historic_climatedata[[3L]][-c(4:6)], all=TRUE, by=c("id", "date")),
    historic_climatedata[[4L]][-c(4:6)], all=TRUE, by=c("id", "date"))
  
  multiFull %>% mutate(year = as.integer(format(date, "%Y")),
                       month = as.integer(strftime(date, '%m')) %% 12, # make December "0"
                       season = cut(month, breaks = c(0, 2, 5, 8, 11),
                                    include.lowest = TRUE,
                                    labels = c("Winter", "Spring", "Summer", "Fall")),
                       year = if_else(month == 0, year + 1L, year)) %>%
    group_by(year, month) %>%
    summarize(tmax_avg = mean(tmax, na.rm = TRUE),
              tmin_avg = mean(tmin, na.rm = TRUE),
              prcp_avg = mean(prcp, na.rm = TRUE),
              prcp_tot = sum(prcp, na.rm = TRUE),
              snwd_avg = mean(snwd, na.rm = TRUE),
              snwd_tot = sum(snwd, na.rm = TRUE))
}

#climate_dc <- tibble(location = "washingtondc", get_climatedata("USC00186350"))
#climate_dc_month <- tibble(location = "washingtondc", get_climatedata("USC00186350"))

historic_data <-
  tibble(location = "washingtondc", get_climatedata("USC00186350")) %>%
  bind_rows(tibble(location = "liestal", get_climatedata("GME00127786"))) %>%
  bind_rows(tibble(location = "kyoto", get_climatedata("JA000047759"))) %>%
  bind_rows(tibble(location = "vancouver", get_climatedata("CA001108395")))

historic_data <- historic_data[historic_data$month %in% c(0:4),] # subset for Dec-Apr data
historic_data <- historic_data %>% 
  pivot_wider(names_from = month, values_from = c(tmax_avg, tmin_avg, prcp_avg, prcp_tot, snwd_avg, snwd_tot))
historic_data <- historic_data %>% full_join(cherry, by = c("location", "year"))

# Location       Years of RNOAA data   Years for Cherry bloom_doy
# Kyoto          1951-2022             812-2021
# Liestal        1953-2022             1894-2021
# Washington DC  1948-2021             1921-2021
# Vancouver      1957-2022             N/A


sunlight <- read.csv("data/avg daily sunlight_dc and whatcom county wa.csv")
str(sunlight)
head(sunlight[sunlight$Month.Code==12,])
sunlight <- sunlight %>% mutate(month = ifelse(Month.Code==12, 0, Month.Code),
                                year = ifelse(Month.Code==12, year_raw+1, year_raw)) %>%
  group_by(location, year, month) %>%
  summarise(sunlight_avg = mean(sunlight_avg, na.rm=TRUE)) %>%
  filter(month %in% c(0:4)) %>%
  pivot_wider(names_from = month, values_from = sunlight_avg, names_prefix = 'sunlight_avg_')

df_final <- historic_data %>% full_join(sunlight, by = c("location", "year")) %>%
  full_join(cherry, by=c("location", "year"))

df_final %>% group_by(location) %>% summarise(initial_year = min(year), final_year = max(year))
write.csv(df_final, file = 'data_JF.csv')

#Missing values by column
sapply(df_final, function(x) sum(is.na(x)))

#####################################################
#  RNOAA data wrangling for Hot and Cold Covariates #
#####################################################

#Getting tmax data for 1950-2021
get_tmax<- function (stationid) {
  ghcnd_search(stationid = stationid, var = c("tmax"), 
               date_min = "1950-01-01", date_max = "2021-02-28")[[1]]}%>%
  mutate(year = as.integer(format(date, "%Y"))) %>%
  group_by(date) %>%
  summarise(tmax)

#Get all four location for 1950-2020
days_max<-
  tibble(location = "washingtondc", get_tmax("USC00186350")) %>%
  bind_rows(tibble(location = "liestal", get_tmax("GME00127786"))) %>%
  bind_rows(tibble(location = "kyoto", get_tmax("JA000047759"))) %>%
  bind_rows(tibble(location = "vancouver", get_tmax("CA001108395")))

#Getting tmin data for 1950-2020
get_tmin<- function (stationid) {
  ghcnd_search(stationid = stationid, var = c("tmin"), 
               date_min = "1950-01-01", date_max = "2021-02-28")[[1]]}%>%
  mutate(year = as.integer(format(date, "%Y"))) %>%
  group_by(date) %>%
  summarise(tmin) 

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
  mutate(year= ifelse(month >= 11, year+1, year )) %>%
  group_by(year, seasons, location) %>%
  mutate(cold= sum(cold, na.rm = TRUE)) %>%
  mutate(hot= sum(hot, na.rm = TRUE)) %>%
  mutate(cold = ifelse(seasons == "Spring", is.na("cold"), cold)) %>%
  mutate(hot = ifelse(seasons == "Winter", is.na("hot"), hot))

#Make separate data frame for later
temp_exp<- temp_all_hot_cold

#selecting important data
temp_all_hot_cold<- select(temp_all_hot_cold, "location", "year", "cold", "hot")

#Grouping data by Year location and season
temp_all_hot_cold<- temp_all_hot_cold %>%
  group_by(year) %>%
  group_by(location, year, seasons) %>% summarize_all(list(~toString(unique(.))))

#For use later on
tempseas<- temp_all_hot_cold

#Erasing season variable
temp_all_hot_cold<- subset(temp_all_hot_cold, select = -c(seasons))

#Transforming cold and hot to numeric so I can aggregate
temp_all_hot_cold<-transform(temp_all_hot_cold, cold= as.numeric(cold), hot= as.numeric(hot))
str(temp_all_hot_cold)

#Aggregating data so that it is by location and have just one row for each year for all locations
temp_all_hot_cold= aggregate(. ~ year + location, data = temp_all_hot_cold, FUN= sum)

########################################################################
# General Multiple Linear Regression Model for Hot and Cold Covariates #
########################################################################

#Merging bloom days for the three locations
cherrytembin<- merge(cherry, temp_all_hot_cold, by= c("location", "year"))

#Getting rid of unnecessary variables
cherrytembin<- select(cherrytembin, -c("alt", "lat", "long", "bloom_date"))

#Fitting linear regression to see it's performance
test_ls_fit<- lm(bloom_doy ~ year*location + cold + hot, data = cherrytembin)
summary(test_ls_fit)

#RSS
sum(resid(test_ls_fit)^2)
deviance(test_ls_fit)
#MSE
mean(test_ls_fit$residuals^2)
#AIC
AIC(test_ls_fit)

#Performs better than demo

#Fitting it for the past predictions for 3 locations
cherrytembin<- cherrytembin %>% 
  bind_cols(predicted_doy = predict(test_ls_fit, newdata= cherrytembin))

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

############################
# Plots and visualizations #
############################

# Plot data by city
cherry %>% 
  filter(year >= 1880) %>%
  ggplot(aes(x = year, y = bloom_doy)) +
  geom_point() +
  geom_step(linetype = 'dotted', color = 'gray50') +
  scale_x_continuous(breaks = seq(1880, 2020, by = 20)) +
  facet_grid(cols = vars(str_to_title(location))) +
  labs(x = "Year", y = "Peak bloom (days since Jan 1st)")

#Plot tmax average for 1950-2020
historic_temperatures %>%
  ggplot() + 
  aes(year, tmax_avg) + 
  geom_line() +
  xlim(1950, 2031) +
  labs(x = "Year", y = "Average maximum temperature (1/10 ?C)") +
  facet_grid(factor(season) ~ str_to_title(location))

#Sunlight only looks promising for March
df_final %>%
  filter(year >= 1880, location == 'washingtondc') %>%
  ggplot(aes(x = sunlight_avg_3, y = bloom_doy)) +
  geom_point() +
  geom_step(linetype = 'dotted', color = 'gray50') +
  labs(x = "Sunlight in Mar (Kj/m^2)", y = "Peak bloom (days since Jan 1st)")

#Temp max avg looks best for Feb/Mar
df_final %>%
  filter(year >= 1880, location != 'vancouver') %>%
  ggplot(aes(x = snwd_tot_4, y = bloom_doy)) +
  geom_point() +
  geom_step(linetype = 'dotted', color = 'gray50') +
  facet_grid(cols = vars(str_to_title(location))) +
  labs(x = "TMAX Avg in Feb", y = "Peak bloom (days since Jan 1st)")

# Plot the predictions alongside the actual observations for 1950 up to 2020.
cherrytembin %>% 
  ggplot(aes(x = year, y = predicted_doy)) +
  geom_line() +
  geom_point(aes(y = bloom_doy)) +
  facet_grid(cols = vars(str_to_title(location))) +
  labs(x = "Year", y = "Peak bloom (days since Jan 1st)")

#Look at all years for different locations and seasons
temp_exp %>% filter() %>%
  ggplot(aes(x= tavg)) + 
  geom_histogram(aes(fill=seasons))+
  facet_wrap(~location + seasons)

#Use the example to look at the average temp in the for the 3 locations for the seasons

#Look at 2019-2020 for locations and split by seasons
temp_exp %>% filter(year>= 2019) %>%
  ggplot(aes(x= tavg)) + 
  geom_histogram(aes(fill=seasons))+
  facet_wrap(~location + seasons + year)

#All QQ plots for all 4 locations
temp_exp %>%
  ggplot(aes(sample= tavg, color = seasons)) + 
  stat_qq()+
  stat_qq_line()+
  facet_wrap(~location)

##################################################################
#  Forecasting for Average Temperature, Hot and Cold Covariates  #
##################################################################

#Predicting the average temperature and then using that to predict the hot and cold variables
temp_exp<- temp_exp %>%
  group_by(year, location, seasons) %>%
  summarise(avgt= mean(tavg, na.rm = TRUE))

#Using a data sets from temp_all_hot
tempseas<-transform(tempseas, cold= as.numeric(cold), hot= as.numeric(hot))

#Merge the two data sets
complete<- merge(temp_exp, tempseas, by = c("location", "year", "seasons"))

##########     Predicting Average Temperature     ################
ls_fit<- lm(avgt~ location* I(year^2)+seasons, data= complete) #.6431
summary(ls_fit)

#RSS
deviance(ls_fit) #132963
#MSE
mean(ls_fit$residuals^2) #263
#AIC
AIC(ls_fit) #4260.21
predict(ls_fit, newdata= complete)

######  Predicting Hot Variable Given the Average Temperature  ############
#Best Multiple Linear Regression
ls_hot<- lm(hot~ avgt + location + seasons * I(year^2), data = complete)
summary(ls_hot) #R^2= .4071

#RSS
deviance(ls_hot) #1578.318
#MSE
mean(ls_hot$residuals^2) #3.131
#AIC
AIC(ls_hot) #2023
predict(ls_hot, newdata= complete)

##########  Plot Predicted vs. Actual for Average Temperature  ################

predict<- complete %>% 
  bind_cols(predicted_avgt = round(predict(ls_fit, newdata= complete)))

#Grouped by locations and seasons predicted vs actual for average temperature
predict %>%
  ggplot(aes(x = year, y=predicted_avgt)) +
  geom_line()+ 
  geom_point(aes(y=avgt)) +
  facet_grid(factor(seasons) ~ str_to_title(location))+
  labs(x = "Year", y = "Average Temperature")

##########  Using Predicted Average Temp to Predict Hot Covariate  ################

#When using "predicted_avgt" in the LR R^2 doesn't reduce by much compared to ls_hot LR
ls_hotp<- lm(hot~ predicted_avgt + location + seasons * I(year^2), data = predict)
summary(ls_hotp) #R^2 = .365

#RSS
deviance(ls_hotp) #1690.574
#MSE
mean(ls_hotp$residuals^2) #3.3543
#AIC
AIC(ls_hotp) #2058


####### Using Predicted Average Temp to Compare to Actual Average Temp.  ########

pred.hot<- predict %>% 
  bind_cols(predicted_hot = round(predict(ls_hotp, newdata= predict)))

# Scatter plot of the actual hot and predicted hot variable using the predicted average temp.
pred.hot %>%
  filter(seasons=="Spring") %>%
  ggplot(aes(x = year, y=predicted_hot)) +
  geom_line()+ 
  geom_point(aes(y=hot)) +
  facet_grid(cols = vars(str_to_title(location)))+
  labs(x = "Year", y = "Number of Days (Hot)")

#### Using the the actual average temp to compare to the actual to predicted hot ####

pred.hot.avg<- predict %>% 
  bind_cols(predicted_hot = round(predict(ls_hot, newdata= predict)))

#Scatter plot of the actual hot and predicted hot variable using the actual average temp.
pred.hot.avg %>%
  filter(seasons=="Spring") %>%
  ggplot(aes(x = year, y=predicted_hot)) +
  geom_line()+ 
  geom_point(aes(y=hot)) +
  facet_grid(cols = vars(str_to_title(location)))+
  labs(x = "Year", y = "Number of Days (Hot)")

##########  Plot Line Graph of ALL the Hot Variables  ################
#Three hot variables using three different techniques 
#"Actual Hot" = The actual hot variable from 1950-2020
#"Predicted Hot = The predicted hot variable using the LR with the actual average temp.
#"Predicted Hot/ Predicted Avg. Temp" = The predicted hot variable using the LR with the predicted average temp. 

#Using the actual average temperature
hot.avg<- complete %>%
  bind_cols(predhot.avg=round(predict(ls_hot, newdata= complete)))

#Using the predicted average temperature
hot.pred<- predict %>%
  bind_cols(predavg.predhot=round(predict(ls_hotp, newdata= predict)))

#Getting all the hot values into one data frame
AllHot<- subset(hot.avg, select= predhot.avg) %>%
  bind_cols(hot.pred)

#Line Plot for comparing the three different hot variables for predicting
AllHot %>%
  filter(seasons=="Spring") %>%
  ggplot(aes(x = year)) + 
  geom_line(aes(y =hot, color= "actual")) +
  geom_line(aes(y= predhot.avg, color ="pred hot"))+
  geom_line(aes(y= predavg.predhot, color ="predict hot/ pred. temp")) + 
  facet_grid(vars(str_to_title(location)))+
  labs(x = "Year", y = "Number of Days")


########  Predicting cold variable in a multiple linear regression  ############
ls_fitcold<- lm(cold~ I(year>=1975)*I(location== "liestal")+ seasons, data = complete)
summary(ls_fitcold) #R^2=.02543
round(predict(ls_fitcold, newdata = comp))


##############  Combining the predicted and full data set  ####################
#Making the extended dates from 2021 to 2032 

complete <- complete %>%
  bind_rows(tibble(location = "kyoto", year= 2022:2032, seasons = "Winter")) %>%
  bind_rows(tibble(location = "kyoto", year= 2022:2032, seasons = "Spring")) %>%
  bind_rows(tibble(location = "liestal", year= 2022:2032, seasons = "Winter")) %>%
  bind_rows(tibble(location = "liestal", year= 2022:2032, seasons = "Spring")) %>%
  bind_rows(tibble(location = "vancouver", year= 2022:2032, seasons = "Winter"))%>%
  bind_rows(tibble(location = "vancouver", year= 2022:2032, seasons = "Spring")) %>%
  bind_rows(tibble(location = "washingtondc", year= 2022:2032, seasons = "Winter")) %>%
  bind_rows(tibble(location = "washingtondc", year= 2022:2032, seasons = "Spring"))

#Predicted average temp and combining data set for prediction for 2022-2032
DF<- complete %>%
  bind_cols(pavgt=(predict(ls_fit, newdata= complete)))

#Created LR using 1950-2020 average temp and then predicted hot using the predicted avg. temp
DF<- DF %>%
  mutate(tavg = avgt) %>%
  mutate(avgt = pavgt) %>%
  bind_cols(phot=(predict(ls_hot, newdata = .)))

##############  Cold Prediction Forecasting  ####################

DF<- DF %>%
  bind_cols(pcold=(predict(ls_fitcold, newdata= DF)))

################################################################################
#####                  Combining Full Data Set                             #####
################################################################################

a<- subset(DF, select= c("location","year","seasons","tavg","cold","hot")) %>%
  filter(year<=2021)

b<- DF %>%
  mutate(pcold = ifelse(seasons == "Spring", is.na("pcold"), pcold)) %>%
  mutate(phot = ifelse(seasons == "Winter", is.na("phot"), phot)) %>%
  filter(year>2021)

b<- subset(b, select= c("location","year","seasons","pavgt","pcold","phot")) %>%
  rename(hot = phot, cold= pcold, tavg= pavgt)

predict<- bind_rows(a, b)

predict<- subset(predict, select= -c(seasons))

Predict<- predict %>%
  group_by(location, year)%>%
  mutate(tavg = mean(tavg), cold = sum(cold), hot= sum(hot))

Predict<- Predict %>%
  group_by(location, year) %>% summarize_all(list(~toString(unique(.)))) %>%
  transform(cold= as.numeric(cold), hot= as.numeric(hot), tavg= as.numeric(tavg))

#Full Data set with forecasting 
djdata<- full_join(df_final, Predict, by= c("year", "location")) 
################################################################################
################################################################################
################################################################################
