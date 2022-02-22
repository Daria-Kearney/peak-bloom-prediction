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


sunlight <- read.csv(file.choose())
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


historic_temperatures %>%
  ggplot() + 
  aes(year, tmax_avg) + 
  geom_line() +
  xlim(1950, 2031) +
  labs(x = "Year", y = "Average maximum temperature (1/10 °C)") +
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

###################
# OLS model: Year #
###################

#Evaluate performance based on absolute difference between predicted dates and observed dates (2011-2021)
iterations = 33
variables = 4
Results <- matrix(ncol = variables, nrow = iterations)

c <- 1
for(i in 2010:2020){
  
  # Fit simple least-squares lines for all sites.
  ls_fit <- lm(bloom_doy ~ location * year, data = cherry, 
               subset = year >= 1880 & year <= i)
  
  # Compute the predictions for all 3 sites
  predictions <- expand_grid(location = unique(cherry$location),
                             year = 1990:2031) %>% 
    bind_cols(predicted_doy = predict(ls_fit, newdata = .))
  
  mergeCols <- c('location','year')
  predictions <- merge(predictions, cherry[,c(-2,-3)], by=mergeCols, all.x = TRUE) %>%
    filter(year==i+1)
  
  #Kyoto
  Results[c,1] <- "kyoto"
  Results[c,2] <- i+1
  Results[c,3] <- predictions$bloom_doy[predictions$location=="kyoto"]
  Results[c,4] <- predictions$predicted_doy[predictions$location=="kyoto"]
  
  #Liestal
  Results[c+1,1] <- "liestal"
  Results[c+1,2] <- i+1
  Results[c+1,3] <- predictions$bloom_doy[predictions$location=="liestal"]
  Results[c+1,4] <- predictions$predicted_doy[predictions$location=="liestal"]
  
  #Washington DC
  Results[c+2,1] <- "washingtondc"
  Results[c+2,2] <- i+1
  Results[c+2,3] <- predictions$bloom_doy[predictions$location=="washingtondc"]
  Results[c+2,4] <- predictions$predicted_doy[predictions$location=="washingtondc"]
  
  c<-c+3
    
}

Results<-data.frame(Results)
colnames(Results) <- c("Location", "Year", "Observed_bloom_doy", "Predicted")
Results$Observed_bloom_doy <- as.numeric(Results$Observed_bloom_doy)
Results$Predicted <- round(as.numeric(Results$Predicted),2)

Results <- Results %>% mutate(Abs_diff=abs(Observed_bloom_doy-Predicted))

Results %>% group_by(Year) %>% summarise(Total_diff=sum(Abs_diff))


#############################################
# OLS model: Spring and Winter avg max temp #
#############################################

# Step 1. extrapolate average seasonal maximum temperature
#ls_fit_temperature <- lm(tmax_avg ~ year * season + location, 
#                         data = historic_temperatures)

#summary(ls_fit_temperature)

#temperature_predictions <-
#  expand_grid(location = c("washingtondc", "liestal", "kyoto", "vancouver" ),
#              season = c("Winter", "Spring", "Summer", "Fall"),
#              year = 1950:2032) %>%
#  bind_cols(predicted_temperature = 
#              predict(ls_fit_temperature, newdata = .)) %>%
#  filter(season %in% c("Winter", "Spring"))

# Step 2. predict bloom day from extrapolated temperatures

#temperature_predictions <- temperature_predictions %>% 
#  left_join(historic_temperatures, by = c("location", "year", "season"))

#temperature_predictions <- temperature_predictions %>%
#  mutate(temp_use=ifelse(is.na(tmax_avg), predicted_temperature, tmax_avg)) %>%
#  select(-c(predicted_temperature, tmax_avg)) %>%
#  pivot_wider(names_from = season, values_from = temp_use)

#temperature_predictions <- temperature_predictions %>% 
#  mutate(SSE=(predicted_temperature-tmax_avg)**2,
#         y_bar=mean(temperature_predictions$tmax_avg, na.rm = TRUE),
#         TSS=(tmax_avg-y_bar)**2)
#(sum(temperature_predictions$TSS, na.rm = TRUE)-sum(temperature_predictions$SSE, na.rm = TRUE))/sum(temperature_predictions$TSS, na.rm = TRUE)
#cor(temperature_predictions$predicted_temperature, temperature_predictions$tmax_avg, use = "complete.obs")**2

#temperature_predictions <- temperature_predictions %>% left_join(cherry, by = c("location", "year"))

#m1 <- lm(bloom_doy ~ Spring * Winter, data = temperature_predictions)
#results_v2 <- predict(m1, newdata = pt1) %>% round() %>% bind_cols(pt1,predicted_bloom_doy=.)


#Evaluate performance based on absolute difference between predicted dates and observed dates (2011-2021)
iterations = 33
variables = 4
Results_V2 <- matrix(ncol = variables, nrow = iterations)

c <- 1
for(i in 2010:2020){
  
  # Step 1. extrapolate average seasonal maximum temperature
  ls_fit_temperature <- lm(tmax_avg ~ year * season + location, 
                           data = historic_temperatures, subset = year >= 1945 & year <= i)
  
  temperature_predictions <-
    expand_grid(location = c("washingtondc", "liestal", "kyoto", "vancouver" ),
                season = c("Winter", "Spring", "Summer", "Fall"),
                year = 1945:2031) %>%
    bind_cols(predicted_temperature = 
                predict(ls_fit_temperature, newdata = .)) %>%
    filter(season %in% c("Winter", "Spring"))
  
  temperature_predictions <- temperature_predictions %>% 
    left_join(historic_temperatures, by = c("location", "year", "season"))
  
  temperature_predictions <- temperature_predictions %>%
    mutate(temp_use=ifelse(is.na(tmax_avg), predicted_temperature, tmax_avg)) %>%
    select(-c(predicted_temperature, tmax_avg)) %>%
    pivot_wider(names_from = season, values_from = temp_use)
  
  temperature_predictions <- temperature_predictions %>% left_join(cherry, by = c("location", "year"))
  
  m1 <- lm(bloom_doy ~ Spring * Winter, data = temperature_predictions)
  
  predictions <- predict(m1, newdata = temperature_predictions) %>% 
    bind_cols(temperature_predictions,predicted_doy=.) %>% filter(year==i+1)
  
  #Kyoto
  Results_V2[c,1] <- "kyoto"
  Results_V2[c,2] <- i+1
  Results_V2[c,3] <- predictions$bloom_doy[predictions$location=="kyoto"]
  Results_V2[c,4] <- predictions$predicted_doy[predictions$location=="kyoto"]
  
  #Liestal
  Results_V2[c+1,1] <- "liestal"
  Results_V2[c+1,2] <- i+1
  Results_V2[c+1,3] <- predictions$bloom_doy[predictions$location=="liestal"]
  Results_V2[c+1,4] <- predictions$predicted_doy[predictions$location=="liestal"]
  
  #Washington DC
  Results_V2[c+2,1] <- "washingtondc"
  Results_V2[c+2,2] <- i+1
  Results_V2[c+2,3] <- predictions$bloom_doy[predictions$location=="washingtondc"]
  Results_V2[c+2,4] <- predictions$predicted_doy[predictions$location=="washingtondc"]
  
  c<-c+3
  
}

Results_V2<-data.frame(Results_V2)
colnames(Results_V2) <- c("Location", "Year", "Observed_bloom_doy", "Predicted")
Results_V2$Observed_bloom_doy <- as.numeric(Results_V2$Observed_bloom_doy)
Results_V2$Predicted <- round(as.numeric(Results_V2$Predicted),2)

Results_V2 <- Results_V2 %>% mutate(Abs_diff=abs(Observed_bloom_doy-Predicted)) %>%
  arrange(Year, Location)

Results_V2 %>% group_by(Year) %>% summarise(Total_diff=sum(Abs_diff))
#write.csv(c(Results, Results_V2), file = 'MAE.csv')

########################
# Comparing OLS models #
########################

out1 <- Results %>% group_by(Year) %>% summarise(Total_diff_v1=sum(Abs_diff))
out2 <- Results_V2 %>% group_by(Year) %>% summarise(Total_diff_v2=sum(Abs_diff))

sum(out1$Total_diff_v1) #201.46
sum(out2$Total_diff_v2) #196.26


out1 %>% left_join(out2, by="Year") %>% mutate(difference=Total_diff_v1-Total_diff_v2)


#################
# Random Forest #
#################

train <- df_final %>% filter(year >= 1950, year <= 2010, location != 'vancouver') %>%
  select(-c(sunlight_avg_0, sunlight_avg_1, sunlight_avg_2, sunlight_avg_3, sunlight_avg_4, lat, long, bloom_date)) %>%
  mutate(location=factor(location))

test <- df_final %>% filter(year == 2011, location != 'vancouver') %>%
  select(-c(sunlight_avg_0, sunlight_avg_1, sunlight_avg_2, sunlight_avg_3, sunlight_avg_4, lat, long, bloom_date)) %>%
  mutate(location=factor(location))

#Impute missing data using randomForest and update with proximity calculations:
train.imputed <- rfImpute(bloom_doy~., data=train, mtry=10, ntree=500) #iter=5 is default number of imputation updates
#rfImpute uses na.roughfix initally to impute using median/mode.


#test.imputed <- rfImpute(bloom_doy~., data=test, mtry=10, ntree=500)
#test.imputed <- rf.unsupervised(data.frame(test %>% select(-c("bloom_doy"))), n=3) # does not impute, needs full data?

train.imputed <- missForest(data.frame(train %>% select(-c("bloom_doy"))))$ximp # NRMSE=0.3165907
train.imputed <- train.imputed %>% left_join(train %>% select(location, year, bloom_doy), by=c("location", "year"))

rf.cherry <- randomForest(bloom_doy~., data=train.imputed, mtry=10, importance=TRUE, ntree=5000, proximity=TRUE)
rf.cherry

test.imputed <- train.imputed %>% bind_rows(test)
test.imputed <- missForest(data.frame(test.imputed %>% select(-c("bloom_doy"))))$ximp
test.imputed <- test.imputed %>% filter(year==2011)

yhat.rf <- predict(rf.cherry, newdata = test.imputed) %>% 
  bind_cols(test.imputed,predicted_doy=.)

mean((yhat.rf$predicted_doy-test$bloom_doy)**2) # test mse = 7.70/8.18 for 2011

#Iterate from 2011-2021
#Evaluate performance based on absolute difference between predicted dates and observed dates (2011-2021)
iterations = 33
variables = 4
Results_V3 <- matrix(ncol = variables, nrow = iterations)

c <- 1
set.seed(634)
for(i in 2010:2020){
  
  train <- df_final %>% filter(year >= 1950, year <= i, location != 'vancouver') %>%
    select(-c(sunlight_avg_0, sunlight_avg_1, sunlight_avg_2, sunlight_avg_3, sunlight_avg_4, lat, long, bloom_date)) %>%
    mutate(location=factor(location))
  
  test <- df_final %>% filter(year == i + 1, location != 'vancouver') %>%
    select(-c(sunlight_avg_0, sunlight_avg_1, sunlight_avg_2, sunlight_avg_3, sunlight_avg_4, lat, long, bloom_date)) %>%
    mutate(location=factor(location))
  
  train.imputed <- rfImpute(bloom_doy~., data=train, mtry=10, ntree=500) #iter=5 is default number of imputation updates
  
  test.imputed <- train.imputed %>% bind_rows(test)
  test.imputed <- missForest(data.frame(test.imputed %>% select(-c("bloom_doy"))))$ximp
  test.imputed <- test.imputed %>% filter(year== i + 1)
  
  rf.cherry <- randomForest(bloom_doy~., data=train.imputed, mtry=10, importance=TRUE, ntree=5000, proximity=TRUE)
  
  predictions <- predict(rf.cherry, newdata = test.imputed) %>% 
    bind_cols(test.imputed,predicted_doy=.) %>% left_join(test %>% select(location, year, bloom_doy), by=c("location", "year"))
  
  #Kyoto
  Results_V3[c,1] <- "kyoto"
  Results_V3[c,2] <- i+1
  Results_V3[c,3] <- predictions$bloom_doy[predictions$location=="kyoto"]
  Results_V3[c,4] <- predictions$predicted_doy[predictions$location=="kyoto"]
  
  #Liestal
  Results_V3[c+1,1] <- "liestal"
  Results_V3[c+1,2] <- i+1
  Results_V3[c+1,3] <- predictions$bloom_doy[predictions$location=="liestal"]
  Results_V3[c+1,4] <- predictions$predicted_doy[predictions$location=="liestal"]
  
  train.dc <- df_final %>% filter(year >= 1950, year <= i, location == 'washingtondc') %>%
    select(-c(lat, long, bloom_date)) %>%
    mutate(location=factor(location))
  
  test.dc <- df_final %>% filter(year == i + 1, location == 'washingtondc') %>%
    select(-c(lat, long, bloom_date)) %>%
    mutate(location=factor(location))
  
  train.imputed.dc <- rfImpute(bloom_doy~., data=train.dc, mtry=10, ntree=500) #iter=5 is default number of imputation updates
  
  rf.cherry.dc <- randomForest(bloom_doy~., data=train.imputed.dc, mtry=10, importance=TRUE, ntree=5000, proximity=TRUE)
  
  test.imputed.dc <- train.imputed.dc %>% bind_rows(test.dc)
  test.imputed.dc <- missForest(data.frame(test.imputed.dc %>% select(-c("bloom_doy"))))$ximp
  test.imputed.dc <- test.imputed.dc %>% filter(year== i + 1)
  
  predictions.dc <- predict(rf.cherry.dc, newdata = test.imputed.dc) %>% 
    bind_cols(test.imputed.dc,predicted_doy=.) %>% left_join(test.dc %>% select(location, year, bloom_doy), by=c("location", "year"))
  
  #Washington DC
  Results_V3[c+2,1] <- "washingtondc"
  Results_V3[c+2,2] <- i+1
  Results_V3[c+2,3] <- predictions.dc$bloom_doy[predictions.dc$location=="washingtondc"]
  Results_V3[c+2,4] <- predictions.dc$predicted_doy[predictions.dc$location=="washingtondc"]
  
  c<-c+3
  
}

Results_V3<-data.frame(Results_V3)
colnames(Results_V3) <- c("Location", "Year", "Observed_bloom_doy", "Predicted")
Results_V3$Observed_bloom_doy <- as.numeric(Results_V3$Observed_bloom_doy)
Results_V3$Predicted <- round(as.numeric(Results_V3$Predicted),2)

Results_V3 <- Results_V3 %>% mutate(Abs_diff=abs(Observed_bloom_doy-Predicted)) %>%
  arrange(Year, Location)

Results_V3 %>% group_by(Year) %>% summarise(Total_diff=sum(Abs_diff))
#write.csv(c(Results_V3), file = 'MAE_seed_634_sunlightdc.csv')

sum(Results_V3$Abs_diff) # 121.01 (seed 635), 122.75 (seed 634)

#####################
# Gradient Boosting #
#####################

train <- df_final %>% filter(year >= 1950, year <= 2010, location != 'vancouver') %>%
  select(-c(lat, long, bloom_date)) %>%
  mutate(location=factor(location))

test <- df_final %>% filter(year == 2011, location != 'vancouver') %>%
  select(-c(lat, long, bloom_date)) %>%
  mutate(location=factor(location))

#Impute missing data using randomForest and update with proximity calculations:
train.imputed <- rfImpute(bloom_doy~., data=train, mtry=10, ntree=500) #iter=5 is default number of imputation updates
#rfImpute uses na.roughfix initally to impute using median/mode.

test.imputed <- train.imputed %>% bind_rows(test)
test.imputed <- missForest(data.frame(test.imputed %>% select(-c("bloom_doy"))))$ximp
test.imputed <- test.imputed %>% filter(year==2011)

#boosting
boost.cherry <- gbm(bloom_doy~., data=train.imputed, distribution = 'gaussian', n.trees = 5000, interaction.depth = 2,
                    shrinkage = 0.001)
summary(boost.cherry) # relative influence plot

yhat.boost <- predict(boost.cherry, newdata = test.imputed)

mean((yhat.boost-test$bloom_doy)**2) # test mse = 4.77

###########################
# 2011 prediction results #
###########################

# d=1, test mse=4.77
# d=2, test mse=4.13
# d=3, test mse=5.96
# d=4, test mse=5.94

# d=1 with sunlight vars, test mse=4.92
# d=2 with sunlight vars, test mse=4.16
# d=3 with sunlight vars, test mse=4.96
# d=4 with sunlight vars, test mse=5.05


#Iterate from 2011-2021
#Evaluate performance based on absolute difference between predicted dates and observed dates (2011-2021)
iterations = 33
variables = 4
Results_V4 <- matrix(ncol = variables, nrow = iterations)

c <- 1
set.seed(634)
for(i in 2010:2020){
  
  train <- df_final %>% filter(year >= 1950, year <= i, location != 'vancouver') %>%
    select(-c(sunlight_avg_0, sunlight_avg_1, sunlight_avg_2, sunlight_avg_3, sunlight_avg_4, lat, long, bloom_date)) %>%
    mutate(location=factor(location))
  
  test <- df_final %>% filter(year == i + 1, location != 'vancouver') %>%
    select(-c(sunlight_avg_0, sunlight_avg_1, sunlight_avg_2, sunlight_avg_3, sunlight_avg_4, lat, long, bloom_date)) %>%
    mutate(location=factor(location))
  
  train.imputed <- rfImpute(bloom_doy~., data=train, mtry=10, ntree=500) #iter=5 is default number of imputation updates
  
  test.imputed <- train.imputed %>% bind_rows(test)
  test.imputed <- missForest(data.frame(test.imputed %>% select(-c("bloom_doy"))))$ximp
  test.imputed <- test.imputed %>% filter(year== i + 1)
  
  boost.cherry <- gbm(bloom_doy~., data=train.imputed, distribution = 'gaussian', n.trees = 5000, interaction.depth = 2,
                      shrinkage = 0.001)
  
  predictions <- predict(boost.cherry, newdata = test.imputed) %>% 
    bind_cols(test.imputed,predicted_doy=.) %>% left_join(test %>% select(location, year, bloom_doy), by=c("location", "year"))
  
  #Kyoto
  Results_V4[c,1] <- "kyoto"
  Results_V4[c,2] <- i+1
  Results_V4[c,3] <- predictions$bloom_doy[predictions$location=="kyoto"]
  Results_V4[c,4] <- predictions$predicted_doy[predictions$location=="kyoto"]
  
  #Liestal
  Results_V4[c+1,1] <- "liestal"
  Results_V4[c+1,2] <- i+1
  Results_V4[c+1,3] <- predictions$bloom_doy[predictions$location=="liestal"]
  Results_V4[c+1,4] <- predictions$predicted_doy[predictions$location=="liestal"]
  
  #Washington DC
  Results_V4[c+2,1] <- "washingtondc"
  Results_V4[c+2,2] <- i+1
  Results_V4[c+2,3] <- predictions$bloom_doy[predictions$location=="washingtondc"]
  Results_V4[c+2,4] <- predictions$predicted_doy[predictions$location=="washingtondc"]
  
  c<-c+3
  
}

Results_V4<-data.frame(Results_V4)
colnames(Results_V4) <- c("Location", "Year", "Observed_bloom_doy", "Predicted")
Results_V4$Observed_bloom_doy <- as.numeric(Results_V4$Observed_bloom_doy)
Results_V4$Predicted <- round(as.numeric(Results_V4$Predicted),2)

Results_V4 <- Results_V4 %>% mutate(Abs_diff=abs(Observed_bloom_doy-Predicted)) %>%
  arrange(Year, Location)

Results_V4 %>% group_by(Year) %>% summarise(Total_diff=sum(Abs_diff))
write.csv(c(Results_V4), file = 'MAE_seed_634_boost.csv')

sum(Results_V4$Abs_diff) # 104.87 (seed 634)

############################
# GAM model and evaluation #
############################

gam_cherry <- gam(bloom_doy ~ location + s(year), data = cherry, 
                  subset = year >= 1880, family = poisson())

gam_cherry <- gam(bloom_doy ~ te(location, year, k=7), data = cherry, 
                  subset = year >= 1880, family = poisson())

plot(gam_cherry)
summary(gam_cherry)



































