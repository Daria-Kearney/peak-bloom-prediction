# Model Analysis Description for Peak Bloom Prediction #

## Objective of README file ##
Assist in the navigation through the main R script names "Peak Bloom Prediction"

# **Table of Contents** #                                       Line Number
Load Packages and Data
    Library ------------------------------------------------------------ 5
    Load cherry data --------------------------------------------------- 20
RNOAA Data Wrangling and Avg Daily Sunlight (KJ/m^2)
    Get_temperature function ------------------------------------------- 33
    Historical_temperature data created -------------------------------- 46
    Get_climate function ----------------------------------------------- 52
    Historic_data created ---------------------------------------------- 78
    Load sunlight data ------------------------------------------------- 96
    Sunlight data cleaning --------------------------------------------- 99
    Df_final data created ---------------------------------------------- 106
RNOAA Data Wrangling for Hot and Cold Covariates 
    Get_tmax function ----------------------------------------------------- 119
    Days_max data created -------------------------------------------------- 127
    Get_tmin function ----------------------------------------------------- 134
    Days_min data created ----------------------------------------- 142
    Tempall data created ------------------------------------------------ 149
    Binary hot/ cold variables created -------------------------------------- 155
    Temp_all_hot_cold data created ------------------------------------------ 161
    Temp_all_hot_cold data cleaning ----------------------------------------- 165
    Temp_exp data created ----------------------------------- 177
    Temp_all_hot_cold data manipulation ----------------------------------- 183
    Tempseas data created ----------------------------------- 188
    aggregating temp_all_hot_cold data -----------------------------------198
Load EPA & NPN Data
    EPA data created 203
    EPA data manipulation 205
Preliminary Plots and Visualizations
    Line plot grid location bloom day vs. year by location 241
    Line plot superimposed location bloom day vs. year 252
    Scatterplot of peak bloom date vs. average tmax for March/Jan. 266
    Line plot of tmax_avg vs year 297
    Step plot of bloom day vs. sunlight for washington dc 306
    Statistic summaries for sunlight data 314
    Step plot of bloom day vs. tmax_avg_2/3 for Feb. & March by location 327
    Histogram of average temp grouped by location and seasons 345
    QQ plot of daily average temp. by location and season 357
EPA Data Exploration 
    Principal Component Analysis 
          for Washington D.C. 372
          for Kyoto 389
          for Liestal 404
    Linear Regression Analysis 
          Testing Models 413
          Scatterplot of predicated vs. actual bloom day 427
    Time Series Analysis
          Estimating Linear Trends 438
          Plots 458
          Scatterplot on lagged variable 471
General Multiple Linear Regression Model for Hot and Cold Covariates
    Fitting Regression 489
    Model Statistic Summary 482
    Model Performance: Calculating Absolute Difference 505
    Scatterplot % Line Plot of Predicted vs. Actual 520
OLS Model
    Fitting linear regression: Predicting Bloom day using Year by Location 531
           Model Performance: Calculating Absolute Difference 576
    Fitting linear regression: Predicting Bloom day by Spring & Winter 587
          Extrapolate average seasonal maximum temperature 597
          Predicting Bloom Day 619
          Model Performance: Calculating Absolute Difference 646
    Comparing OLS Model 659
Building Random Forest Model

          
            

    
    
    

























IMPORTANT Notes***
Change line count!!
MD - file doesn't look good in GitHub but does look okay in RMD and plain text file if need be.!!! If open project then Readme file looks nice. 
R notebook bad idea!










