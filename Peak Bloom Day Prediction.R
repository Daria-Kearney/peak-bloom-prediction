############################################################################################################
############################################################################################################
################################################################################################################################################
#Daria's Data Set
#Creating data set for Cold and Warm Buckets- Daria
library(rnoaa)
library(tidyverse)
stations <- ghcnd_stations()

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

#Erasing season variable
temp_all_hot_cold<- subset(temp_all_hot_cold, select = -c(seasons))

#Transforming cold and hot to numeric so I can aggregate
temp_all_hot_cold<-transform(temp_all_hot_cold, cold= as.numeric(cold), hot= as.numeric(hot))
str(temp_all_hot_cold)

#Aggregating data so that it is by location and have just one row for each year for all locations
temp_all_hot_cold= aggregate(. ~ year + location, data = temp_all_hot_cold, FUN= sum)

#Observing general multiple linear regression model
#Merging bloom days for the three locations note: takes away Vancouver days
cherry <- read.csv("data/washingtondc.csv") %>% 
  bind_rows(read.csv("data/liestal.csv")) %>% 
  bind_rows(read.csv("data/kyoto.csv"))

cherrytembin<- merge(cherry, temp_all_hot_cold, by= c("location", "year"))

#Getting rid of unnecessary variables
cherrytembin<- select(cherrytembin, -c("alt", "lat", "long", "bloom_date"))

#Fitting linear regression to see how it performs
test_ls_fit<- lm(bloom_doy ~ year*location + cold + hot, data = cherrytembin)
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

############################################################################################################
############################################################################################################
############################################################################################################
##Jeremy's Data/ R Code
# Load data
cherry <- read.csv("data/washingtondc.csv") %>% 
  bind_rows(read.csv("data/liestal.csv")) %>% 
  bind_rows(read.csv("data/kyoto.csv"))

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
    summarise(tmax_avg = mean(tmax, na.rm = TRUE),
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

sunlight <- sunlight %>% mutate(month = ifelse(Month.Code==12, 0, Month.Code),
                                year = ifelse(Month.Code==12, year_raw+1, year_raw)) %>%
  group_by(location, year, month) %>%
  summarise(sunlight_avg = mean(sunlight_avg, na.rm=TRUE)) %>%
  filter(month %in% c(0:4)) %>%
  pivot_wider(names_from = month, values_from = sunlight_avg, names_prefix = 'sunlight_avg_')

df_final <- historic_data %>% full_join(sunlight, by = c("location", "year")) %>%
  full_join(cherry, by=c("location", "year"))

df_final %>% group_by(location) %>% summarise(initial_year = min(year), final_year = max(year))

djdata<- full_join(df_final, temp_all_hot_cold, by= c("year", "location")) 

############################################################################################################
############################################################################################################
################################################################################################################################################
#Extrapolate for Average Temperature 
#Exploratory Data Analysis
#Daria's Data/R Code
#Looking at distribution for 4 locations for for the two seasons from 2000-2020
#Checking normality
library(ggplot2)
################################################################################
################################################################################
################################################################################
#Graph
#fix graph but looking at distribution
#Looking at 2020 for locations and split by seasons
temp_exp %>% filter(year>= 2020) %>%
  ggplot(aes(x= tavg)) + 
  geom_histogram(aes(fill=seasons))+
  facet_wrap(~location + seasons)

#Use the example to look at the average temp in the for the 3 locations for the seasons

#Look at 2019-2020 for locations and split by seasons
#Checking normality
temp_exp %>% filter(year>= 2019) %>%
  ggplot(aes(x= tavg)) + 
  geom_histogram(aes(fill=seasons))+
  facet_wrap(~location + seasons + year)

#Look at all years for different locations and seasons

temp_exp %>% filter() %>%
  ggplot(aes(x= tavg)) + 
  geom_histogram(aes(fill=seasons))+
  facet_wrap(~location + seasons)

#QQ plot for all locations separate
temp_exp %>% filter(location=="washingtondc")%>%
  ggplot(aes(sample= tavg, color = seasons)) + 
  stat_qq()+
  stat_qq_line()
temp_exp %>% filter(location=="kyoto")%>%
  ggplot(aes(sample= tavg, color = seasons)) + 
  stat_qq()+
  stat_qq_line()
temp_exp %>% filter(location=="liestal")%>%
  ggplot(aes(sample= tavg, color = seasons)) + 
  stat_qq()+
  stat_qq_line()
temp_exp %>% filter(location=="vancouver")%>%
  ggplot(aes(sample= tavg, color = seasons)) + 
  stat_qq()+
  stat_qq_line()

#All QQ plots for all 4 locations
temp_exp %>%
  ggplot(aes(sample= tavg, color = seasons)) + 
  stat_qq()+
  stat_qq_line()+
  facet_wrap(~location)

################################################################################
################################################################################
########################            Extrapolation            ###################
################################################################################
################################################################################
#Predicting the average temperature and then using that to predict the hot and cold variables
temp_exp<- temp_exp %>%
  group_by(year, location, seasons) %>%
  summarise(avgt= mean(tavg, na.rm = TRUE))

#Using a data sets from temp_all_hot
tempseas<-transform(tempseas, cold= as.numeric(cold), hot= as.numeric(hot))

#Merge the two data sets
complete<- merge(temp_exp, tempseas, by = c("location", "year", "seasons"))


#####     Predicting average temperature     ######
ls_fit<- lm(avgt~ location* I(year^2)+seasons, data= complete) #.6431
summary(ls_fit)

#RSS
deviance(ls_fit) #132963
#MSE
mean(ls_fit$residuals^2) #263
#AIC
AIC(ls_fit) #4260.21
predict(ls_fit, newdata= complete)

######  Predicting hot variable given the predicted average temperature  #########
#Best Multiple Linear Regression
#R^2= .4071
ls_hot<- lm(hot~ avgt + location + seasons * I(year^2), data = complete)
summary(ls_hot)

#RSS
deviance(ls_hot) #1578.318
#MSE
mean(ls_hot$residuals^2) #3.131
#AIC
AIC(ls_hot) #2023
predict(ls_hot, newdata= complete)
################################################################################################
################################################################################################
######     Plot predicted vs actual for average temperature for each location      #######
predict<- complete %>% 
  bind_cols(predicted_avgt = round(predict(ls_fit, newdata= complete)))

#Grouped by locations and seasons predicted vs actual for average temperature
predict %>%
  ggplot(aes(x = year, y=predicted_avgt)) +
  geom_line()+ 
  geom_point(aes(y=avgt)) +
  facet_grid(factor(seasons) ~ str_to_title(location))+
  labs(x = "Year", y = "Average Temperature")

#################################################################################
#################################################################################
## Use the predicted average temperature from lm to predict the hot variable ####
ls_hot<- lm(hot~ avgt + location + seasons * I(year^2), data = complete)
summary(ls_hot) #R^2 = .4071
predict(ls_hot, newdata= complete)

#######
#When using "predicted_avgt" in the linear regression R^2 doesn't reduce by much
ls_hotp<- lm(hot~ predicted_avgt + location + seasons * I(year^2), data = predict)
summary(ls_hotp) #R^2 = .365
#RSS
deviance(ls_hotp) #1690.574
#MSE
mean(ls_hotp$residuals^2) #3.3543
#AIC
AIC(ls_hotp) #2058
#################################################################################
#################################################################################
## Using the predicted average temperature to see how it differs from true temp #
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

#######    Using the the actual average temp to see how it differs   ######### 
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

################################################################################
##Plotting all the 3 line graph for the predicted and actual number of hot days#
#Using the actual average temperature
hot.avg<- complete %>%
  bind_cols(predhot.avg=round(predict(ls_hot, newdata= complete)))

#Using the predicted average temperature
hot.pred<- predict %>%
  bind_cols(predavg.predhot=round(predict(ls_hotp, newdata= predict)))

rm(AllHot)

#Getting all the hot values into one data frame
AllHot<- subset(hot.avg, select= predhot.avg) %>%
  bind_cols(hot.pred)

#Comparing the different hot variables for predicting
AllHot %>%
  filter(seasons=="Spring") %>%
  ggplot(aes(x = year)) + 
  geom_line(aes(y =hot, color= "actual")) +
  geom_line(aes(y= predhot.avg, color ="pred hot"))+
  geom_line(aes(y= predavg.predhot, color ="predict hot/ pred. temp")) + 
  facet_grid(vars(str_to_title(location)))+
  labs(x = "Year", y = "Number of Days")

##################################################################################
##################################################################################
######  Predicting cold variable in a multiple linear  #########
ls_fitcold<- lm(cold~ I(year>=1975)*I(location== "liestal")+ seasons, data = complete)
summary(ls_fitcold) #R^2=.02543
round(predict(ls_fitcold, newdata = comp))

##################################################################################
##################################################################################
#Putting together the predicted and full data set
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

#Creating lm using 1950-2020 avg. temp and then predicting using predicted avg. temp
DF<- DF %>%
  mutate(tavg = avgt) %>%
  mutate(avgt = pavgt) %>%
  bind_cols(phot=(predict(ls_hot, newdata = .)))

#######################################
#Cold Prediction

DF<- DF %>%
  bind_cols(pcold=(predict(ls_fitcold, newdata= DF)))

##################################################################################
##################################################################################
###################################################################################
###############       Combining Data         ####################################
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

djdata<- full_join(df_final, Predict, by= c("year", "location")) 

################################################################################
################################################################################
###############################################################################
