##########################
# Load Packages and Data #
##########################

library(tidyverse)
library(lubridate)
library(mgcv) # gam function for splines
library(rnoaa)
library(geosphere) # distm function for euclidean distance
library(randomForest)
library(missForest) # missForest()
library(gbm) # gbm()
library(Hmisc) #prcomp()
library(astsa) #lag.1plot & acf()
library(dynlm)
library(gridExtra) #grid.arrange()
library(ggpubr) #ggarrange()
library(grid) #textGrob( )

# Load data
cherry <- read.csv("data/washingtondc.csv") %>% 
  bind_rows(read.csv("data/liestal.csv")) %>% 
  bind_rows(read.csv("data/kyoto.csv"))

########################################################
# RNOAA Data Wrangling and Avg Daily Sunlight (KJ/m^2) #
########################################################

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
    summarise(tmax_avg = mean(tmax, na.rm = TRUE))
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
    #summarize()
    summarise(tmax_avg = mean(tmax, na.rm = TRUE),
              tmin_avg = mean(tmin, na.rm = TRUE),
              prcp_avg = mean(prcp, na.rm = TRUE),
              prcp_tot = sum(prcp, na.rm = TRUE),
              snwd_avg = mean(snwd, na.rm = TRUE),
              snwd_tot = sum(snwd, na.rm = TRUE))
}

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

sunlight <- read.csv("data/sunlight.csv")
#str(sunlight)
#head(sunlight[sunlight$Month.Code==12,])
sunlight <- sunlight %>% mutate(month = ifelse(Month.Code==12, 0, Month.Code),
                                year = ifelse(Month.Code==12, year_raw+1, year_raw)) %>%
  group_by(location, year, month) %>%
  summarise(sunlight_avg = mean(sunlight_avg, na.rm=TRUE)) %>%
  filter(month %in% c(0:4)) %>%
  pivot_wider(names_from = month, values_from = sunlight_avg, names_prefix = 'sunlight_avg_')

df_final <- historic_data %>% full_join(sunlight, by = c("location", "year"))

df_final %>% group_by(location) %>% summarise(initial_year = min(year), final_year = max(year))
#write.csv(df_final, file = 'data_JF.csv')

#Missing values by column
sapply(df_final, function(x) sum(is.na(x)))

#####################################################
#  RNOAA Data Wrangling for Hot and Cold Covariates #
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

#Creating binary variable label "cold" for when average temperature is <= -115 (1/10 °C)= 11°F
tempall$cold<- ifelse(tempall$tavg <= -115, 1, 0)

#Creating binary variable label "warm" for when average temperature is <= 100(1/10 °C)= 50°F
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

################################
# Load EPA Data & Manipulation #
################################

EPA <- read.csv("data/EPA.csv")

######  Data Manipulation  ######

# Find the minimum year of available data, bind to cherry dataset by that year 
min(EPA$year)
data <- left_join(dplyr::filter(cherry, year >= 1990)
                  ,EPA,by="year")

# Impute missing values in EPA dataset with average values
data$Carbon.dioxide <- impute(data$Carbon.dioxide, mean)
data$Methane <- impute(data$Methane,mean)
data$Nitrous.oxide <- impute(data$Nitrous.oxide,mean)
data$HFCs..PFCs..SF6..and.NF3 <- impute(data$HFCs..PFCs..SF6..and.NF3,mean)
data$Anomaly <- impute(data$Anomaly,mean)
data$Winter <- impute(data$Winter,mean)
data$Spring <- impute(data$Spring,mean)
data$Summer <- impute(data$Summer,mean)
data$Fall <- impute(data$Fall,mean)
data$Earth.s.surface <- impute(data$Earth.s.surface,mean)
data$Lower.troposphere..measured.by.satellite...UAH. <- impute(
  data$Lower.troposphere..measured.by.satellite...UAH., mean
)
data$Lower.troposphere..measured.by.satellite...RSS. <- impute(
  data$Lower.troposphere..measured.by.satellite...RSS., mean
)

# Renaming the last two to be better naming conventions
names(data)[18:19] <- c("UAH","RSS")

# Removing data that isn't relevant
data1 <- select(data,!c("lat","long","bloom_date"))

########################################
# Preliminary Plots and Visualizations #
########################################

# Line plot by location of peak bloom days vs. year
cherry %>% 
  filter(year >= 1880) %>%
  ggplot(aes(x = year, y = bloom_doy)) +
  geom_point() +
  geom_step(linetype = 'dotted', color = 'gray50') +
  scale_x_continuous(breaks = seq(1880, 2020, by = 20)) +
  facet_wrap(~location, labeller = labeller(location=
    c("kyoto" = "Kyoto", "liestal" = "Liestal- Weideli","vancouver"="Vancouver", "washingtondc"= "Washington D.C.")))+
  labs(x = "Year", y = "Peak bloom (days since Jan 1st)", title = "Bloom Days vs. Year grouped by Location")

#Line Plot for years vs. Bloom Day
cherry %>% filter(year >= 1950) %>%
  ggplot(aes(year,bloom_doy, color=location)) +
  geom_line(aes(),size =.8) +
  labs(x="Year",
       y="Days from January 1st",
       color = "Location",
       caption = "Figure 1: Peak bloom days starting January 1st of three different locations from 1950 to 2021") +
  scale_colour_manual(labels = c("Washington D.C.", "Kyoto", "Liestal- Weideli"),
                      values = c("#0072B2","#E69F00", "#009E73")) + 
  theme_minimal(base_line_size = 1, base_rect_size = 1) +
  theme(plot.title =element_text(hjust = 0.5), 
        plot.caption = element_text(hjust=0)) 

##### Scatterplot of Average Tmax by Months vs Peak Bloom Date #####

overlayline<- df_final %>%
  group_modify(function (.x, .y){
    avg1_ls<- lm(bloom_doy~ tmax_avg_1, data =df_final)
    avg3_ls<- lm(bloom_doy~ tmax_avg_3, data =df_final)
    res<- tibble(tmax_avg_1 = df_final$tmax_avg_1, tmax_avg_3 = df_final$tmax_avg_3)
    res$predict.bloom_doy1<- predict(avg1_ls, newdata= .)
    res$predict.bloom_doy3<- predict(avg3_ls, newdata= .)
    res
  })

colors<- c("January"="#0072B2", "March" = "#E69F00")

df_final %>% 
  ggplot(aes(y = bloom_doy))+ 
  geom_point(aes(x= tmax_avg_1, colour = "January"))+
  geom_point(aes(x= tmax_avg_3, colour= "March"))+
  geom_line(data = overlayline, aes(x = tmax_avg_1, y=predict.bloom_doy1, colour ="January"))+ 
  geom_line(data = overlayline, aes(x = tmax_avg_3, y=predict.bloom_doy3, colour = "March"))+
  labs(x="Max Temperature (1/10 °C)",
       y="Days from January 1st",
       title="Peak Bloom Days versus Max Temperature",
       colour = "Months",
       caption = "Figure 2: Peak bloom days and average maximum temperature for January and March overlayed with a linear regression 
predicting peak bloom given max. temperature.")+ 
  scale_color_manual(values= colors) + 
  theme_minimal(base_line_size = 1, base_rect_size = 1)+
  theme(plot.title =element_text(hjust = 0.5), 
        plot.caption = element_text(hjust=0))

#Plot tmax average for 1950-2020
historic_temperatures %>%
  ggplot() + 
  aes(year, tmax_avg) + 
  geom_line() +
  xlim(1950, 2031) +
  labs(x = "Year", y = "Average maximum temperature (1/10 °C)",
       title = "Average Max Temperature vs. Year") +
  facet_grid(factor(season) ~ str_to_title(location))

#Sunlight only looks promising for March
df_final %>%
  filter(year >= 1880, location == 'washingtondc') %>%
  ggplot(aes(x = sunlight_avg_3, y = bloom_doy)) +
  geom_point() +
  geom_step(linetype = 'dotted', color = 'gray50') +
  labs(x = "Sunlight in Mar (Kj/m^2)", y = "Peak bloom (days since Jan 1st)",
       title= "Bloom Day vs. Average Sunlight in March")

###############  Statistic Summaries  ###############
cor(df_final$bloom_doy, df_final$sunlight_avg_0, use="complete.obs") # -0.27
cor(df_final$bloom_doy, df_final$sunlight_avg_1, use="complete.obs") # -.32
cor(df_final$bloom_doy, df_final$sunlight_avg_2, use="complete.obs") # -.07
cor(df_final$bloom_doy, df_final$sunlight_avg_3, use="complete.obs") # -.37
cor(df_final$bloom_doy, df_final$sunlight_avg_4, use="complete.obs") # .14

summary(lm(bloom_doy~sunlight_avg_0, data=df_final[df_final$location=='washingtondc',])) # p-value > .05
summary(lm(bloom_doy~sunlight_avg_1, data=df_final[df_final$location=='washingtondc',])) # p-value > .05
summary(lm(bloom_doy~sunlight_avg_2, data=df_final[df_final$location=='washingtondc',])) # p-value > .05
summary(lm(bloom_doy~sunlight_avg_3, data=df_final[df_final$location=='washingtondc',])) # p-value < .05
summary(lm(bloom_doy~sunlight_avg_4, data=df_final[df_final$location=='washingtondc',])) # p-value > .05

#Temp max avg looks best for Feb/Mar
df_final %>%
  filter(year >= 1880, location != 'vancouver') %>%
  ggplot(aes(x = tmax_avg_2, y = bloom_doy)) +
  geom_point() +
  geom_step(linetype = 'dotted', color = 'gray50') +
  facet_wrap(~location, labeller = labeller(location=
    c("kyoto" = "Kyoto", "liestal" = "Liestal- Weideli","vancouver"="Vancouver", "washingtondc"= "Washington D.C.")))+
  labs(x = "Average Max Temperature in Feb. (1/10 °C)", y = "Peak bloom (days since Jan 1st)",
       title= "Bloom Day vs. Average Maximum Temperature in Feb. by Location")

df_final %>%
  filter(year >= 1880, location != 'vancouver') %>%
  ggplot(aes(x = tmax_avg_3, y = bloom_doy)) +
  geom_point() +
  geom_step(linetype = 'dotted', color = 'gray50') +
  facet_wrap(~location, labeller = labeller(location=
    c("kyoto" = "Kyoto", "liestal" = "Liestal- Weideli","vancouver"="Vancouver", "washingtondc"= "Washington D.C.")))+
  labs(x = "Average Max Temperature in March (1/10 °C)", y = "Peak bloom (days since Jan 1st)", 
       title ="Peak Bloom Day vs. Average Max Temperature in March by Location")

#Look at all years for different locations and seasons
temp_exp %>% filter() %>%
  ggplot(aes(x= tavg)) + 
  geom_histogram(aes(fill=seasons))+
  labs(x = "Average Temperature (1/10 °C)", y = "Count", 
       title = "Distribution of Average Temperature grouped by Location and Season", fill = "Seasons") +
  facet_wrap(~location +seasons, labeller = labeller(location=
   c("kyoto" = "Kyoto", "liestal" = "Liestal- Weideli","vancouver"="Vancouver", "washingtondc"= "Washington D.C.")))

#Look at 2019-2020 for locations and grouped by seasons
temp_exp %>% filter(year>= 2019) %>%
  ggplot(aes(x= tavg)) + 
  labs(x = "Average Temperature (1/10 °C)", y = "Count", title = "Distribution of Average Temperature grouped by Location and Season for 2019", fill= "Seasons")+
  geom_histogram(aes(fill=seasons))+
  facet_wrap(~location +seasons, labeller = labeller(location=
    c("kyoto" = "Kyoto", "liestal" = "Liestal- Weideli","vancouver"="Vancouver", "washingtondc"= "Washington D.C.")))

#All QQ plots for all 4 locations daily average temp.
temp_exp %>%
  ggplot(aes(sample= tavg, color = seasons)) + 
  stat_qq()+
  stat_qq_line()+
  labs(x = "Theoretical Quantiles", y="Standarized residuals", title = "Q-Q plot of Daily Average Temperature grouped 
                         by Location and Season",
       color = "Seasons")+
  facet_wrap(~location, labeller = labeller(location=
    c("kyoto" = "Kyoto", "liestal" = "Liestal- Weideli","vancouver"="Vancouver", "washingtondc"= "Washington D.C.")))

########################################################
#                EPA Data Exploration                  #
########################################################

dc <- data1 %>% filter(location=="washingtondc")

# Removing altitude since it is zero and can't be scaled
dc1 <- select(dc,-c(location,alt))

###########  Principal Components Analysis  ###########

#Principal Components Analysis for Washingtondc
pr.dc <- prcomp(dc1,scale=TRUE)
pr.dc$rotation
biplot(pr.dc,scale=0)

# Note, in the biplot of the first two principal components, the variables
# Nitrous.oxide, bloom day, and anomaly are clustered together. Seasons are grouped
#   by the season that follow each other (spring-winter, fall-summer)

kyoto <- data1 %>% filter(location=="kyoto")

# Removing altitude since it is all the same and non-numeric column
kyoto1 <- select(kyoto,-c(location,alt))

#Principal Components Analysis for Kyoto
pr.kyoto <- prcomp(kyoto1,scale=TRUE)
pr.kyoto$rotation
biplot(pr.kyoto,scale=0)
pr.var.kyoto <- pr.kyoto$sdev^2
pr.var.kyoto
pve.kyoto <- pr.var.kyoto/sum(pr.var.kyoto)
pve.kyoto

# Similar factpattern as the DC results

liestal <- data1 %>% filter(location=="liestal")

liestal1 <- select(liestal,-c(location,alt))

#Principal Components Analysis for Liestal
pr.liestal <- prcomp(liestal1,scale=TRUE)
pr.liestal$rotation
par(mfrow=c(1,3))
biplot(pr.dc,scale=0)
biplot(pr.kyoto,scale=0)
biplot(pr.liestal,scale=0)

####################  Linear regression  ####################
#Using uncorrelated variables: nitrous oxide, methane,carbon dioxide
ls_fit.epa <- lm(bloom_doy ~ Carbon.dioxide + Methane + Nitrous.oxide +
                   location * year, data = data1)
summary(ls_fit.epa)

predict(ls_fit.epa)

predictions.epa <- data1 %>% 
  bind_cols(predicted_doy = predict(ls_fit.epa, newdata = .))

par(mfrow=c(1,1))

######### Scatter plot of predicted bloom day versus actual bloom day ###########
# Simple graph to see if high predicted values occur with high actual values
predictions.epa %>% 
  ggplot(aes(x = bloom_doy, y = predicted_doy,
             color=location)) +
  geom_point() +
  labs(x = "Bloom Day Since Jan 1st", y = "Predicted Peak Day Since Jan 1st",
       title = "Scatterplot of Predicted bloom day vs. actual bloom day")

# Predictive error
mean(abs(predictions.epa$predicted_doy - predictions.epa$bloom_doy)/predictions.epa$bloom_doy)*100

######################  Time Series Analysis  ######################
options(scipen = 100)
# Estimating linear trend in all locations, prior to 2000 for forecasting
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

####  Plots for Time Series for all three locations  ######

plot(diff(kyoto_bloom), type="o")
mean(diff(kyoto_bloom)) # drift estimate = .002
acf(diff(dc_bloom), 48)

plot(diff(liestal_bloom), type="o")
mean(diff(liestal_bloom)) # drift estimate = -0.114
acf(diff(liestal_bloom), 48)

plot(diff(dc_bloom), type="o")
mean(diff(dc_bloom)) # drift estimate = .205
acf(diff(dc_bloom), 48)
#All plots suggest lagged 1 variable is statistically significant 

#######  Scatter plot on lagged variables  #######

lag1.plot(kyoto_bloom,4)
lag1.plot(liestal_bloom,4)
lag1.plot(dc_bloom, 4)
#Scatter plot on the lagged variables shows no influence of non-linear relationships

########################################################################
# General Multiple Linear Regression Model for Hot and Cold Covariates #
########################################################################

#Merging bloom days for the three locations
cherrytembin<- merge(cherry, temp_all_hot_cold, by= c("location", "year"))

#Getting rid of unnecessary variables
cherrytembin<- select(cherrytembin, -c("alt", "lat", "long", "bloom_date"))

#Fitting linear regression to see it's performance
test_ls_fit<- lm(bloom_doy ~ year*location + cold + hot, data = cherrytembin)
summary(test_ls_fit) #R^2 = .324

#RSS
deviance(test_ls_fit) #9699.71
#MSE
mean(test_ls_fit$residuals^2) #46.19
#AIC
AIC(test_ls_fit) #1418.83
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
#The mean over the 70 years is 15.26 which performs better than demo
mean(diff$sum) #15.26
sum(diff$sum) #167.9

# Plot the predictions alongside the actual observations for 1950 up to 2020.
cherrytembin %>% 
  ggplot(aes(x = year, y = predicted_doy)) +
  geom_line() +
  geom_point(aes(y = bloom_doy)) +
  labs(x = "Year", y = "Peak bloom (days since Jan 1st)",
  caption = 
"Figure 8: Scatterplot of actual bloom days. The lines show the predicted bloom days as obtained from the linear regression model.")+
  facet_wrap(~location, labeller = labeller(location=
    c("kyoto" = "Kyoto", "liestal" = "Liestal- Weideli","vancouver"="Vancouver", "washingtondc"= "Washington D.C.")))+
  theme_minimal(base_line_size = 1, base_rect_size = 1)+
  theme(plot.title =element_text(hjust = 0.5), 
        plot.caption = element_text(hjust=0))

#############
# OLS Model #
#############
################  OLS Model: Year by Location  ################
#Iterate from 2011-2021
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

sum(Results$Abs_diff) # test error = 201.46

############  OLS Model: Spring and Winter Avg Max. Temp.  ############
#Iterate from 2011-2021
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

sum(Results_V2$Abs_diff) # test error = 196.26

#################  Comparing OLS models  #################

out1 <- Results %>% group_by(Year) %>% summarise(Total_diff_v1=sum(Abs_diff))
out2 <- Results_V2 %>% group_by(Year) %>% summarise(Total_diff_v2=sum(Abs_diff))

# Model                         # Test error
# lm(bloom_doy~year*location)     201.46
# lm(bloom_doy~Spring*Winter)     196.26

out1 %>% left_join(out2, by="Year") %>% mutate(difference=Total_diff_v1-Total_diff_v2)

################################
# Building Random Forest Model #
################################

#Add in Hot and Cold variables
df_final <- df_final %>% left_join(temp_all_hot_cold, by=c('location', 'year'))

train <- df_final %>% filter(year >= 1950, year <= 2010, location != 'vancouver') %>%
  select(-c(sunlight_avg_0, sunlight_avg_1, sunlight_avg_2, sunlight_avg_3, sunlight_avg_4, lat, long, bloom_date)) %>%
  mutate(location=factor(location))

test <- df_final %>% filter(year == 2011, location != 'vancouver') %>%
  select(-c(sunlight_avg_0, sunlight_avg_1, sunlight_avg_2, sunlight_avg_3, sunlight_avg_4, lat, long, bloom_date)) %>%
  mutate(location=factor(location))

#Impute missing data using randomForest and update with proximity matrix:
set.seed(634)
train.imputed <- rfImpute(bloom_doy~., data=train, mtry=10, ntree=500) #iter=5 is default number of imputation updates

rf.cherry <- randomForest(bloom_doy~., data=train.imputed, mtry=10, importance=TRUE, ntree=5000, proximity=TRUE)
rf.cherry

test.imputed <- train.imputed %>% bind_rows(test)
test.imputed <- missForest(data.frame(test.imputed %>% select(-c("bloom_doy"))))$ximp
test.imputed <- test.imputed %>% filter(year==2011)

yhat.rf <- predict(rf.cherry, newdata = test.imputed) %>% 
  bind_cols(test.imputed,predicted_doy=.)

mean((yhat.rf$predicted_doy-test$bloom_doy)**2) # test mse = 7.94 for 2011

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
    select(-c(lat, long, bloom_date, sunlight_avg_0, sunlight_avg_1, sunlight_avg_2, sunlight_avg_4)) %>%
    mutate(location=factor(location))
  
  test.dc <- df_final %>% filter(year == i + 1, location == 'washingtondc') %>%
    select(-c(lat, long, bloom_date, sunlight_avg_0, sunlight_avg_1, sunlight_avg_2, sunlight_avg_4)) %>%
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

sum(Results_V3$Abs_diff)

# Mar sunlight for DC and hot/cold vars, test error = 118.86
# Dec-Apr sunlight for DC, test error = 122.67

###################################
# Building Gradient Boosted Model #
###################################

train <- df_final %>% filter(year >= 1950, year <= 2010, location != 'vancouver') %>%
  select(-c(sunlight_avg_0, sunlight_avg_1, sunlight_avg_2, sunlight_avg_4, lat, long, bloom_date)) %>%
  mutate(location=factor(location))

test <- df_final %>% filter(year == 2011, location != 'vancouver') %>%
  select(-c(sunlight_avg_0, sunlight_avg_1, sunlight_avg_2, sunlight_avg_4, lat, long, bloom_date)) %>%
  mutate(location=factor(location))

#Impute missing data using randomForest and update with proximity matrix:
set.seed(634)
train.imputed <- rfImpute(bloom_doy~., data=train, mtry=10, ntree=500) #iter=5 is default number of imputation updates

test.imputed <- train.imputed %>% bind_rows(test)
test.imputed <- missForest(data.frame(test.imputed %>% select(-c("bloom_doy"))))$ximp
test.imputed <- test.imputed %>% filter(year==2011)

#Boosting
set.seed(634)
boost.cherry <- gbm(bloom_doy~., data=train.imputed, distribution = 'gaussian', n.trees = 5000, interaction.depth = 2,
                    shrinkage = 0.001)
summary(boost.cherry) # relative influence plot

yhat.boost <- predict(boost.cherry, newdata = test.imputed)%>% 
  bind_cols(test.imputed,predicted_doy=.) %>% left_join(test %>% select(location, year, bloom_doy), by=c("location", "year"))

mean((yhat.boost$predicted_doy-test$bloom_doy)**2) # test mse = 3.51

# test mse = 74.67 imputing all covariates in test set for 2011-2021

#sum(abs(yhat.boost$predicted_doy-yhat.boost$bloom_doy))
# test mas = 234.81 imputing all covariates in test set for 2011-2021

############################  2011 prediction results  ############################

# d=1, test mse=4.75
# d=2, test mse=3.51
# d=3, test mse=4.73
# d=4, test mse=4.27

#Iterate from 2011-2021
#Evaluate performance based on absolute difference between predicted dates and observed dates (2011-2021)
iterations = 33
variables = 4
Results_V4 <- matrix(ncol = variables, nrow = iterations)

c <- 1
set.seed(634)
for(i in 2010:2020){
  
  train <- df_final %>% filter(year >= 1950, year <= i, location != 'vancouver') %>%
    select(-c(sunlight_avg_0, sunlight_avg_1, sunlight_avg_2, sunlight_avg_3, sunlight_avg_4, lat, long, 
              bloom_date, snwd_avg_4, snwd_tot_4)) %>%
    mutate(location=factor(location))
  
  test <- df_final %>% filter(year == i + 1, location != 'vancouver') %>%
    select(-c(sunlight_avg_0, sunlight_avg_1, sunlight_avg_2, sunlight_avg_3, sunlight_avg_4, lat, long, 
              bloom_date, snwd_avg_4, snwd_tot_4)) %>%
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
#write.csv(c(Results_V4), file = 'MAE_seed_634_boost.csv')

sum(Results_V4$Abs_diff) # test error = 104.07

##############################################################
# Gradient Boosting with Separate Model for DC - FINAl Model #
##############################################################

#Iterate from 2011-2021
#Evaluate performance based on absolute difference between predicted dates and observed dates (2011-2021)
iterations = 33
variables = 4
Results_V5 <- matrix(ncol = variables, nrow = iterations)

c <- 1
set.seed(634)
for(i in 2010:2020){
  
  train <- df_final %>% filter(year >= 1950, year <= i, location != 'vancouver') %>%
    select(-c(sunlight_avg_0, sunlight_avg_1, sunlight_avg_2, sunlight_avg_3, sunlight_avg_4, lat, long, bloom_date,
              snwd_avg_4, snwd_tot_4, cold)) %>%
    mutate(location=factor(location))
  
  test <- df_final %>% filter(year == i + 1, location != 'vancouver') %>%
    select(-c(sunlight_avg_0, sunlight_avg_1, sunlight_avg_2, sunlight_avg_3, sunlight_avg_4, lat, long, bloom_date,
              snwd_avg_4, snwd_tot_4, cold)) %>%
    mutate(location=factor(location))
  
  #impute using median/mode and update using proximity matrix over 5 iterations
  train.imputed <- rfImpute(bloom_doy~., data=train, mtry=10, ntree=500)
  
  #combine test rows with training to impute missing data
  test.imputed <- train.imputed %>% bind_rows(test)
  test.imputed <- missForest(data.frame(test.imputed %>% select(-c("bloom_doy"))))$ximp
  test.imputed <- test.imputed %>% filter(year== i + 1)
  
  boost.cherry <- gbm(bloom_doy~., data=train.imputed, distribution = 'gaussian', n.trees = 5000, interaction.depth = 2,
                      shrinkage = 0.001)
  
  predictions <- predict(boost.cherry, newdata = test.imputed) %>% 
    bind_cols(test.imputed,predicted_doy=.) %>% left_join(test %>% select(location, year, bloom_doy), by=c("location", "year"))
  
  #Kyoto
  Results_V5[c,1] <- "kyoto"
  Results_V5[c,2] <- i+1
  Results_V5[c,3] <- predictions$bloom_doy[predictions$location=="kyoto"]
  Results_V5[c,4] <- predictions$predicted_doy[predictions$location=="kyoto"]
  
  #Liestal
  Results_V5[c+1,1] <- "liestal"
  Results_V5[c+1,2] <- i+1
  Results_V5[c+1,3] <- predictions$bloom_doy[predictions$location=="liestal"]
  Results_V5[c+1,4] <- predictions$predicted_doy[predictions$location=="liestal"]
  
  train.dc <- df_final %>% filter(year >= 1950, year <= i, location == 'washingtondc') %>%
    select(-c(sunlight_avg_0, sunlight_avg_1, sunlight_avg_2, sunlight_avg_4, lat, long, bloom_date, location,
              snwd_avg_4, snwd_tot_4, cold))
  
  test.dc <- df_final %>% filter(year == i + 1, location == 'washingtondc') %>%
    select(-c(sunlight_avg_0, sunlight_avg_1, sunlight_avg_2, sunlight_avg_4, lat, long, bloom_date, location,
              snwd_avg_4, snwd_tot_4, cold))
  
  train.imputed.dc <- rfImpute(bloom_doy~., data=train.dc, mtry=10, ntree=500)
  
  test.imputed.dc <- train.imputed.dc %>% bind_rows(test.dc)
  test.imputed.dc <- missForest(data.frame(test.imputed.dc %>% select(-c("bloom_doy"))))$ximp
  test.imputed.dc <- test.imputed.dc %>% filter(year== i + 1)
  
  boost.cherry.dc <- gbm(bloom_doy~., data=train.imputed.dc, distribution = 'gaussian', n.trees = 5000, interaction.depth = 2,
                         shrinkage = 0.001)
  
  predictions.dc <- predict(boost.cherry.dc, newdata = test.imputed.dc) %>% 
    bind_cols(test.imputed.dc,predicted_doy=.) %>% left_join(test.dc %>% select(year, bloom_doy), by=c("year"))
  
  #Washington DC
  Results_V5[c+2,1] <- "washingtondc"
  Results_V5[c+2,2] <- i+1
  Results_V5[c+2,3] <- predictions.dc$bloom_doy[1]
  Results_V5[c+2,4] <- predictions.dc$predicted_doy[1]
  
  c<-c+3
  
}

Results_V5<-data.frame(Results_V5)
colnames(Results_V5) <- c("Location", "Year", "Observed_bloom_doy", "Predicted")
Results_V5$Observed_bloom_doy <- as.numeric(Results_V5$Observed_bloom_doy)
Results_V5$Predicted <- round(as.numeric(Results_V5$Predicted),2)

Results_V5 <- Results_V5 %>% mutate(Abs_diff=abs(Observed_bloom_doy-Predicted)) %>%
  arrange(Year, Location)

Results_V5 %>% group_by(Year) %>% summarise(Total_diff=sum(Abs_diff))
#write.csv(c(Results_V5), file = 'MAE_seed_634_boost_2272022.csv')

sum(Results_V5$Abs_diff) #100.61

########################  Test Error  ########################

## using seed 634 ##
## Using hot (Jan/Feb) & cold (Nov/Dec) counts ##

# 100.61 with hot and Mar sunlight for DC, no snwd_avg/tot_4
# 106.60 with cold, hot and Mar sunlight
# 104.61 with cold, hot and Mar sunlight for DC
# 104.13 with cold, hot and Mar sunlight for DC, no snwd_avg/tot_4

# 104.07 with cold, hot added
# 105.04 with cold, hot added and sunlight for DC
# 105.36 with cold, hot added and Mar sunlight for DC
# 106.48 with cold, hot added and only 1979 forward data for DC with sunlight
# 104.16 with cold, hot added and only 1979 forward data for DC with Mar sunlight only

## using seed 634 ##
## Using hot (Feb/Mar/Apr) & cold (Nov/Dec/Jan) counts ##

# 113.09 with cold, hot added

######################  GAM model testing  ######################

gam_cherry <- gam(bloom_doy ~ location + s(year), data = cherry, 
                  subset = year >= 1880, family = poisson())

plot(gam_cherry)
summary(gam_cherry)

##################################################################
#  Forecasting for Average Temperature, Hot and Cold Covariates  #
##################################################################

#Predicting the average temperature and then using that to predict the hot and cold variables
temp_exp<- temp_exp %>%
  group_by(year, location, seasons) %>%
  summarise(avgt= mean(tavg, na.rm = TRUE))

#All QQ plots for all 4 locations for the average temp by season
temp_exp %>%
  ggplot(aes(sample= avgt, color = seasons)) + 
  stat_qq()+
  stat_qq_line()+
  facet_wrap(~location, labeller = labeller(location=
    c("kyoto" = "Kyoto", "liestal" = "Liestal- Weideli","vancouver"="Vancouver", "washingtondc"= "Washington D.C.")))+
  labs(x = "Theoretical Quantiles", y="Standarized residuals",
       title= "Q-Q Plots for Seasonal Average Temperature", color = "Seasons",
  caption= "Figure 5: Quantile-Quantile plots of seasonal average temperature from 1950 to 2020 for four different locations.") +
  scale_colour_manual(labels = c("Spring", "Winter"),
                      values = c("#009e73","#56B4E9")) +
  theme_minimal()+
  theme(plot.title =element_text(hjust = 0.5), 
        plot.caption = element_text(hjust=0))
  
#Using a data sets from temp_all_hot
tempseas<-transform(tempseas, cold= as.numeric(cold), hot= as.numeric(hot))

#Merge the two data sets
complete<- merge(temp_exp, tempseas, by = c("location", "year", "seasons"))

#Three graphs looking at relationship of temperature, hot, and bloom day
#Scatter plot of hot vs. average temp.
complete %>%
  filter(seasons=="Spring") %>%
  ggplot(aes(x = avgt, y= hot, color = location))+ 
  geom_point()+
  geom_smooth(method = "lm", se = FALSE)+ 
  theme_minimal()+
  labs(x= "Average Temperature (1/10 °C)", y= "Number of Hot Days",
       title = "Number of Hot Days versus Average Temperature by Location", 
       color = "Location",
       caption = "Figure 3: The number of hot days in Spring (Jan./Feb.) versus the average temperature (1/10 °C) 
in Spring with a loess linear smoothing line for the four different locations.")+
  scale_colour_manual(labels = c("Vancouver", "Kyoto", "Liestal- Weideli", "Washington D.C."),
                    values = c("#0072B2","#E69F00", "#009E73", "#F0E442"))+
  theme(plot.title =element_text(hjust = 0.5), 
        plot.caption = element_text(hjust=0))

#Loess Smooth Juxtaposed Average temp and Hot with Bloom Date for just Spring
p<-complete %>%
  filter(seasons=="Spring") %>%
  merge(cherry,. , by= c("location", "year")) %>%
  ggplot(aes(x= avgt, y=bloom_doy, color = location))+
  geom_smooth(method = "lm")+
  theme_minimal()+
  theme(legend.position= "none")+
  labs(x = "Average Temperature (1/10 °C)", y = "Days from January 1st")+
  scale_colour_manual(labels = c("Kyoto", "Liestal- Weideli", "Washington D.C."),
                      values = c("#0072B2","#E69F00", "#009E73"))

q<- complete %>%
  filter(seasons=="Spring") %>%
  merge(cherry,. , by= c("location", "year")) %>%
  ggplot(aes(x = hot, y=bloom_doy, color = (str_to_title(location))))+
  geom_smooth(method = "lm")+
  theme_minimal()+
  labs(x= "Number of Hot Days", y = "Days from January 1st", color = "Location")+
  scale_colour_manual(labels = c("Kyoto", "Liestal- Weideli", "Washington D.C."),
                      values = c("#0072B2","#E69F00", "#009E73"))

grid.arrange(p, q, ncol=2, top = "Comparing Average Temperature and Number of Days with Peak Bloom Day", 
bottom= textGrob("Figure 4: A loess smooth juxtaposed graph comparing average temp. and number of hot days with the peak bloom day for Spring.", 
                 gp= gpar(fontsize =9), just = "center"))

##############  Predicting Average Temperature  ##############
ls_fit<- lm(avgt~ location* I(year^2)+seasons, data= complete) #R^2 = .6431
summary(ls_fit)

#RSS
deviance(ls_fit) #132963.1
#MSE
mean(ls_fit$residuals^2) #263.82
#AIC
AIC(ls_fit) #4260.21
predict(ls_fit, newdata= complete)

########  Predicting Hot Variable Given the Average Temperature  ########
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

#Grouped by locations and seasons predicted and actual temp by year
predict %>%
  ggplot(aes(x = year, y=predicted_avgt)) +
  geom_line()+ 
  geom_point(aes(y=avgt)) +
  facet_grid(factor(seasons) ~ str_to_title(location))+
  labs(x = "Year", y = "Average Temperature (1/10 °C)")

##########  Using Predicted Average Temp to Predict Hot Covariate  ##########

#When using "predicted_avgt" in the LR R^2 doesn't reduce by much compared to ls_hot LR
ls_hotp<- lm(hot~ predicted_avgt + location + seasons * I(year^2), data = predict)
summary(ls_hotp) #R^2 = .365

#RSS
deviance(ls_hotp) #1690.57
#MSE
mean(ls_hotp$residuals^2) #3.35
#AIC
AIC(ls_hotp) #2058.26

#######  Using Predicted Average Temp to Compare to Actual Average Temp.  #######

pred.hot<- predict %>% 
  bind_cols(predicted_hot = round(predict(ls_hotp, newdata= predict)))

# Scatter plot of the actual hot and predicted hot variable using the predicted average temp.
pred.hot %>%
  filter(seasons=="Spring") %>%
  ggplot(aes(x = year, y=predicted_hot)) +
  geom_line()+ 
  geom_point(aes(y=hot)) +
  facet_grid(vars(location), labeller = labeller(location=
   c("kyoto" = "Kyoto", "liestal" = "Liestal- Weideli","vancouver"="Vancouver", "washingtondc"= "Washington D.C.")))+
  labs(x = "Year", y = "Number of Hot Days",
       title="Scatterplot of Predicted Hot vs. Actual Hot 
       by Location Using the Predicted Average Temperature.")

#######  Using the actual average temp to compare to the actual to predicted hot  #######

pred.hot.avg<- predict %>% 
  bind_cols(predicted_hot = round(predict(ls_hot, newdata= predict)))

#Scatter plot of the actual hot and predicted hot variable using the actual average temp.
pred.hot.avg %>%
  filter(seasons=="Spring") %>%
  ggplot(aes(x = year, y=predicted_hot)) +
  geom_line()+ 
  geom_point(aes(y=hot)) +
  facet_grid(vars(location), labeller = labeller(location=
  c("kyoto" = "Kyoto", "liestal" = "Liestal- Weideli","vancouver"="Vancouver", "washingtondc"= "Washington D.C.")))+
  labs(x = "Year", y = "Number of Hot Days",
       title = "Scatterplot of Predicted Hot vs. Actual Hot by Location
       Using the Actual Average Temperature")

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
  geom_line(aes(y =hot, color= "Actual Hot Value")) +
  geom_line(aes(y= predhot.avg, color ="Predicted Hot Value"))+
  geom_line(aes(y= predavg.predhot, color ="Predict Hot|
  Predicted Temp.")) + 
  facet_grid(vars(location), labeller = labeller(location=
    c("kyoto" = "Kyoto", "liestal" = "Liestal- Weideli","vancouver"="Vancouver", "washingtondc"= "Washington D.C.")))+
  scale_color_manual(name= "Methods",
                     labels= c("Actual Hot Value", "Predicted Hot Value", "Predict Hot|Predicted Temp."),
                     values=c("#D55E00", "#009E73", "#56B4E9")) + 
  labs(x = "Year", y = "Number of Hot Days", color = "Methods", 
       captions= "Figure 7: Line plot comparing the three different methods of predicting the number of hot days.")+
  theme_minimal()+
  theme(legend.key.size = unit(.5, "cm"), legend.key.height = unit(.5, "cm"), 
        legend.key.width = unit(1, "cm"), legend.text = element_text(size =8))

########  Predicting cold variable in a multiple linear regression  ############
ls_fitcold<- lm(cold~ I(year>=1975)*I(location== "liestal")+ seasons, data = complete)
summary(ls_fitcold) #R^2=.02543
round(predict(ls_fitcold, newdata = complete))

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

# Cold Prediction Forecasting

DF<- DF %>%
  bind_cols(pcold=(predict(ls_fitcold, newdata= DF)))

################################################################################
#                      Combining Full Data Set                                 #
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

##################  Full Data Set with Hot/Cold Forecasting  ##################

df_final<- select(df_final, -c("cold", "hot"))

complete_df<- full_join(df_final, Predict, by = c("year", "location"))

############################################################################
# MC simulation to forecast remaining covariates and 2023-2031 bloom dates #
############################################################################
#Q-Q plot for Normality
q1<- df_final %>%
  ggplot(aes(sample= tmax_avg_3, shape = location, color = str_to_title(location))) + 
  stat_qq()+
  stat_qq_line()+
  labs(x = "Theoretical Quantiles", y="Standarized residuals",
       title = "Average Max. Temperature in March")+
  scale_color_manual(name= "Location",
                     labels= c("Kyoto", "Liestal-Weideli", 'Vancouver', "Washington D.C."),
                     values=c("#D55E00", "#009E73", "#56B4E9", "#CC79A7")) + 
  scale_shape_manual(name= "Location",
                     labels= c("Kyoto", "Liestal-Weideli", 'Vancouver', "Washington D.C."),
                     values=c(15, 5, 17, 4))+
  theme_minimal()+
  theme(plot.title =element_text(hjust = 0.5, size = 12))

q2<- df_final %>%
  ggplot(aes(sample= tmax_avg_2, shape = location, color = str_to_title(location))) + 
  stat_qq()+
  stat_qq_line()+
  labs(x = "Theoretical Quantiles", y="Standarized residuals",
       title = "Average Max. Temperature in Feb.")+
  scale_color_manual(name= "Location",
                     labels= c("Kyoto", "Liestal-Weideli", 'Vancouver', "Washington D.C."),
                     values=c("#D55E00", "#009E73", "#56B4E9", "#CC79A7")) + 
  scale_shape_manual(name= "Location",
                     labels= c("Kyoto", "Liestal-Weideli", 'Vancouver', "Washington D.C."),
                     values=c(15, 5, 17, 4))+
  theme_minimal()+
  theme(plot.title =element_text(hjust = 0.5, size = 12))

q3<- df_final %>%
  ggplot(aes(sample= snwd_avg_2, shape = location, color = str_to_title(location))) + 
  stat_qq()+
  stat_qq_line() + 
  labs(x = "Theoretical Quantiles", y="Standarized residuals",
       title = "Average Snow Fall in Feb.")+
  scale_color_manual(name= "Location",
                     labels= c("Kyoto", "Liestal-Weideli", 'Vancouver', "Washington D.C."),
                     values=c("#D55E00", "#009E73", "#56B4E9", "#CC79A7")) + 
  scale_shape_manual(name= "Location",
                     labels= c("Kyoto", "Liestal-Weideli", 'Vancouver', "Washington D.C."),
                     values=c(15, 5, 17, 4))+
  theme_minimal()+
  theme(plot.title =element_text(hjust = 0.5, size = 12))

q4<- df_final %>%
  ggplot(aes(sample= tmin_avg_3, shape = location, color = str_to_title(location))) + 
  stat_qq()+
  stat_qq_line()+
  labs(x = "Theoretical Quantiles", y="Standarized residuals",
       title = "Average Min. Temperature in March", color = "Location")+
  scale_color_manual(name= "Location",
                     labels= c("Kyoto", "Liestal-Weideli", 'Vancouver', "Washington D.C."),
                     values=c("#D55E00", "#009E73", "#56B4E9", "#CC79A7")) + 
  scale_shape_manual(name= "Location",
                     labels= c("Kyoto", "Liestal-Weideli", 'Vancouver', "Washington D.C."),
                     values=c(15, 5, 17, 4))+
  theme_minimal()+
  theme(plot.title =element_text(hjust = 0.5, size = 12))

#Creating grid of all 4 Q-Q plots
plot<- ggarrange(q1, q2, q3, q4, ncol =2, nrow =2, common.legend= TRUE, legend = "right")
annotate_figure(plot, bottom = text_grob(
        'Figure 6:Q-Q plots of four forecasted variables from the different locations.', just ="center", size = 10))

###################################  Forecast  #################################
df.forecast <- complete_df %>% filter(year >= 1950) %>%
  select(-c(lat, long, bloom_date, sunlight_avg_0, sunlight_avg_1, sunlight_avg_2, 
            sunlight_avg_4, snwd_tot_4, snwd_avg_4, cold, tavg))

temperature_predictions <-
  expand_grid(location = c("washingtondc", "liestal", "kyoto", "vancouver" ),
              year = 2023:2031) %>%
  left_join(df.forecast, by=c('location', 'year')) %>%
  add_column(sigma_tmax_0=NA, .after = "tmax_avg_0") %>%
  add_column(sigma_tmax_1=NA, .after = "tmax_avg_1") %>%
  add_column(sigma_tmax_2=NA, .after = "tmax_avg_2") %>%
  add_column(sigma_tmax_3=NA, .after = "tmax_avg_3") %>%
  add_column(sigma_tmax_4=NA, .after = "tmax_avg_4") %>%
  add_column(sigma_tmin_0=NA, .after = "tmin_avg_0") %>%
  add_column(sigma_tmin_1=NA, .after = "tmin_avg_1") %>%
  add_column(sigma_tmin_2=NA, .after = "tmin_avg_2") %>%
  add_column(sigma_tmin_3=NA, .after = "tmin_avg_3") %>%
  add_column(sigma_tmin_4=NA, .after = "tmin_avg_4") %>%
  add_column(sigma_sunlight_3=NA, .after = "sunlight_avg_3") %>%
  mutate(alt=ifelse(location %in% c('washingtondc', 'vancouver'), 0,
                    ifelse(location == 'liestal', 350,
                           ifelse(location == 'kyoto', 44, NA))))

varlist1 <- c('tmax_avg_0', 'tmax_avg_1', 'tmax_avg_2', 'tmax_avg_3', 'tmax_avg_4',
              'tmin_avg_0', 'tmin_avg_1', 'tmin_avg_2', 'tmin_avg_3', 'tmin_avg_4',
              'prcp_avg_0', 'prcp_avg_1', 'prcp_avg_2', 'prcp_avg_3', 'prcp_avg_4',
              'prcp_tot_0', 'prcp_tot_1', 'prcp_tot_2', 'prcp_tot_3', 'prcp_tot_4',
              'snwd_avg_0', 'snwd_avg_1', 'snwd_avg_2', 'snwd_avg_3',
              'snwd_tot_0', 'snwd_tot_1', 'snwd_tot_2', 'snwd_tot_3', 'sunlight_avg_3')

# Regress monthly temperatures, precipitation, snowfall depth, and March sunlight (DC & Vancouver)
# on year and location
for (i in 1:length(varlist1)){
  
  if(grepl('max', varlist1[i]) | grepl('min', varlist1[i])){
    mod <- as.formula(paste0(varlist1[i], "~year+location"))
    m1_glm <- glm(mod, data=df.forecast)
    j <- which(colnames(temperature_predictions)==varlist1[i])
    
    temperature_predictions[,j] <- predict(m1_glm, temperature_predictions)
    temperature_predictions[,j+1] <- sqrt(m1_glm$deviance/m1_glm$df.residual)
    
    print(paste(varlist1[i], sqrt(m1_glm$deviance/m1_glm$df.residual)))
  }
  else if(grepl('sun', varlist1[i])){
    mod <- as.formula(paste0(varlist1[i], "~year+location"))
    m1_glm <- glm(mod, data=df.forecast)
    j <- which(colnames(temperature_predictions)==varlist1[i])
    
    temperature_predictions[temperature_predictions$location %in% c('washingtondc', 'vancouver'),j] <- predict(m1_glm, temperature_predictions[temperature_predictions$location %in% c('washingtondc', 'vancouver'),])
    temperature_predictions[temperature_predictions$location %in% c('washingtondc', 'vancouver'),j+1] <- sqrt(m1_glm$deviance/m1_glm$df.residual)
    
    print(paste(varlist1[i], sqrt(m1_glm$deviance/m1_glm$df.residual)))
  }
  else{
    mod <- as.formula(paste0("round(", varlist1[i], ")", "~year+location"))
    m2_glm <- glm(mod, data=df.forecast, family = 'poisson')
    
    j <- which(colnames(temperature_predictions)==varlist1[i])
    temperature_predictions[,j] <- predict(m2_glm, temperature_predictions)
  }
  
}

# Generate predicted bloom doy distribution by year and location (2023-2031)
Results_forecast <- data.frame()

set.seed(634)
for(i in 1:1000){
  
  bloom_forecast <-
    temperature_predictions %>%
    rowwise() %>%
    mutate(tmax_avg_0 = rnorm(1, tmax_avg_0, sigma_tmax_0),
           tmax_avg_1 = rnorm(1, tmax_avg_1, sigma_tmax_1),
           tmax_avg_2 = rnorm(1, tmax_avg_2, sigma_tmax_2),
           tmax_avg_3 = rnorm(1, tmax_avg_3, sigma_tmax_3),
           tmax_avg_4 = rnorm(1, tmax_avg_4, sigma_tmax_4),
           tmin_avg_0 = rnorm(1, tmin_avg_0, sigma_tmin_0),
           tmin_avg_1 = rnorm(1, tmin_avg_1, sigma_tmin_1),
           tmin_avg_2 = rnorm(1, tmin_avg_2, sigma_tmin_2),
           tmin_avg_3 = rnorm(1, tmin_avg_3, sigma_tmin_3),
           tmin_avg_4 = rnorm(1, tmin_avg_4, sigma_tmin_4),
           prcp_avg_0 = rpois(1, prcp_avg_0), prcp_avg_1 = rpois(1, prcp_avg_1),
           prcp_avg_2 = rpois(1, prcp_avg_2), prcp_avg_3 = rpois(1, prcp_avg_3),
           prcp_avg_4 = rpois(1, prcp_avg_4), prcp_tot_0 = rpois(1, prcp_tot_0),
           prcp_tot_1 = rpois(1, prcp_tot_1), prcp_tot_2 = rpois(1, prcp_tot_2),
           prcp_tot_3 = rpois(1, prcp_tot_3), prcp_tot_4 = rpois(1, prcp_tot_4),
           snwd_avg_0 = rpois(1, snwd_avg_0), snwd_avg_1 = rpois(1, snwd_avg_1),
           snwd_avg_2 = rpois(1, snwd_avg_2), snwd_avg_3 = rpois(1, snwd_avg_3),
           snwd_tot_0 = rpois(1, snwd_tot_0), snwd_tot_1 = rpois(1, snwd_tot_1),
           snwd_tot_2 = rpois(1, snwd_tot_2), snwd_tot_3 = rpois(1, snwd_tot_3),
           sunlight_avg_3 = rnorm(1, sunlight_avg_3, sigma_sunlight_3),
           iteration=i)
  
  predictions <- predict(boost.cherry, newdata = bloom_forecast %>% filter(location %in% c('liestal', 'kyoto')) %>%
                           select(-c(sunlight_avg_3, sigma_sunlight_3))) %>% 
    bind_cols(bloom_forecast %>% filter(location %in% c('liestal', 'kyoto')) %>%
                select(-c(sunlight_avg_3, sigma_sunlight_3)),predicted_doy=.)
  
  predictions.dc <- predict(boost.cherry.dc, newdata = bloom_forecast %>% filter(location %in% c('vancouver', 'washingtondc'))) %>% 
    bind_cols(bloom_forecast %>% filter(location %in% c('vancouver', 'washingtondc')), predicted_doy=.)
  
  Results_forecast <- rbind(Results_forecast, predictions %>% select(location, year, predicted_doy, iteration))
  Results_forecast <- rbind(Results_forecast, predictions.dc %>% select(location, year, predicted_doy, iteration))
  }

##########################################
# Generate predicted bloom date for 2022 #
##########################################

df.past <- complete_df %>% filter(year >= 1950, year <= 2022) %>%
  select(-c(sunlight_avg_0, sunlight_avg_1, sunlight_avg_2, sunlight_avg_4, lat, long, bloom_date,
            snwd_avg_4, snwd_tot_4, cold, tavg, bloom_doy)) %>%
  mutate(location=factor(location),
         alt=ifelse(is.na(alt) & location %in% c('vancouver', 'washingtondc'), 0,
                    ifelse(is.na(alt) & location=='liestal', 350, 
                           ifelse(is.na(alt) & location=='kyoto', 44, alt))))

set.seed(634)
df.2022 <- missForest(data.frame(df.past))$ximp
df.2022 <- df.2022 %>% filter(year==2022)

predictions.2022 <- predict(boost.cherry, newdata=df.2022 %>% filter(location %in% c('liestal', 'kyoto'))) %>%
  bind_cols(df.2022 %>% filter(location %in% c('liestal', 'kyoto')), predicted_doy=.)
predictions.2022.dc <- predict(boost.cherry.dc, newdata=df.2022 %>% filter(location %in% c('vancouver', 'washingtondc'))) %>%
  bind_cols(df.2022 %>% filter(location %in% c('vancouver', 'washingtondc')), predicted_doy=.)

output <- Results_forecast %>% group_by(location, year) %>% 
  summarise(iterations=n(), min_bloom_doy=min(predicted_doy),
            median_bloom_doy=median(predicted_doy), mean_bloom_doy=mean(predicted_doy),
            max_bloom_doy=max(predicted_doy), predicted_doy=mean(predicted_doy)) %>%
  rbind(predictions.2022 %>% select(location, year, predicted_doy)) %>%
  rbind(predictions.2022.dc %>% select(location, year, predicted_doy))

#write.csv(output, file='forecast_1000.csv')


