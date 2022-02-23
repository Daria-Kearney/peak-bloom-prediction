library(tidyverse)
library(Hmisc)

# Data pull
# Working directory is GitHub repository


cherry <- read.csv("data/washingtondc.csv") %>% 
  bind_rows(read.csv("data/kyoto.csv")) %>% 
  bind_rows(read.csv("data/liestal.csv"))

phenomtrics <- read.csv("data/USA-NPN_individual_phenometrics_data.csv")
intensity <- read.csv("data/USA-NPN_status_intensity_observations_data.csv")
phenom_dict <- read.csv("data/USA-NPN_individual_phenometrics_datafield_descriptions.csv")
int_dict <- read.csv("data/USA-NPN_status_intensity_datafield_descriptions.csv")

EPA <- read.csv("data/combined_EPA_dataset.csv")

# Find the minimum year of available data, bind
#    to cherry dataset by that year 
min(EPA$year)
data <- left_join(dplyr::filter(cherry, year >= 1990)
          ,EPA,by="year")
View(data)

## Impute missing values in EPA dataset with average values --- this data wasn't recorded
##  for those years yet so also may be necessary to exclude


# imputing median value with NA 
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

# Renaming the last two to be better naming conventions, they differ only in 
## measurement type

names(data)[18:19] <- c("UAH","RSS")


# Removing data that isn't relevant
data1 <- select(data,!c("lat","long","bloom_date"))
View(data1)

# Data exploration - PCA - DC
dc <- data1 %>% filter(location=="washingtondc")

# Removing altitude since it is zero and can't be scaled
dc1 <- select(dc,-c(location,alt))

pr.dc <- prcomp(dc1,scale=TRUE)
pr.dc$rotation
biplot(pr.dc,scale=0)

# Note, in the biplot of the first two principal components, the variables
# Nitrous.oxide, bloom day, and anomaly are clustered together. Seasons are grouped
#   by the season that follow each other (spring-winter, fall-summer)

kyoto <- data1 %>% filter(location=="kyoto")

# Removing altitude since it is all the same and non-numeric column
kyoto1 <- select(kyoto,-c(location,alt))

pr.kyoto <- prcomp(kyoto1,scale=TRUE)
pr.kyoto$rotation
biplot(pr.kyoto,scale=0)

pr.var.kyoto <- pr.kyoto$sdev^2
pr.var.kyoto
pve.kyoto <- pr.var/sum(pr.var.kyoto)
pve.kyoto

# Similar factpattern as the DC results

liestal <- data1 %>% filter(location=="liestal")

# Similar factpattern as the DC results
liestal1 <- select(liestal,-c(location,alt))

pr.liestal <- prcomp(liestal1,scale=TRUE)
pr.liestal$rotation


par(mfrow=c(1,3))
biplot(pr.dc,scale=0)
biplot(pr.kyoto,scale=0)
biplot(pr.liestal,scale=0)

## Linear regression using uncorrelated variables: nitrus oxide, methane,
##   carbon dioxide

ls_fit <- lm(bloom_doy ~ Carbon.dioxide + Methane + Nitrous.oxide +
               location * year, data = data1)
summary(ls_fit)

predict(ls_fit)


predictions <- data1 %>% 
  bind_cols(predicted_doy = predict(ls_fit, newdata = .))

par(mfrow=c(1,1))

## Simple graph to see if high predicted values occur with high actual values
predictions %>% 
  ggplot(aes(x = bloom_doy, y = predicted_doy,
             color=location)) +
  geom_point() +
  labs(x = "Bloom Day Since Jan 1st", y = "Predicted Peak Day Since Jan 1st")

# Predictive error
mean(abs(predictions$predicted_doy - predictions$bloom_doy)/predictions$bloom_doy)*100
write.csv(predictions,"predictions.csv")
