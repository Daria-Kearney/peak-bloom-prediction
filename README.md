# peak-bloom-prediction
Template repository for the GMU cherry blossom peak bloom prediction competition.
# Model Analysis Description for Peak Bloom Prediction #

## Objective of README file ##
Assist in the navigation through the main R script named "Peak Bloom Prediction"

# **Table of Contents** #                                                                Line Number
Load Packages and Data
    Library ---------------------------------------------------------------------------------- 5
    Load cherry data ------------------------------------------------------------------------ 20

RNOAA Data Wrangling and Avg Daily Sunlight (KJ/m^2)
    Get_temperature function ---------------------------------------------------------------- 33
    Historical_temperature data created ----------------------------------------------------- 46
    Get_climate function -------------------------------------------------------------------- 52
    Historic_data created ------------------------------------------------------------------- 78
    Load sunlight data ---------------------------------------------------------------------- 96
    Sunlight data cleaning ------------------------------------------------------------------ 99
    Df_final data created ------------------------------------------------------------------ 106

RNOAA Data Wrangling for Hot and Cold Covariates 
    Get_tmax function ---------------------------------------------------------------------- 119
    Days_max data created ------------------------------------------------------------------ 127
    Get_tmin function ---------------------------------------------------------------------- 134
    Days_min data created ------------------------------------------------------------------ 142
    Tempall data created ------------------------------------------------------------------- 149
    Binary hot/ cold variables created ----------------------------------------------------- 155
    Temp_all_hot_cold data created --------------------------------------------------------- 161
    Temp_all_hot_cold data cleaning -------------------------------------------------------- 165
    Temp_exp data created ------------------------------------------------------------------ 177
    Temp_all_hot_cold data manipulation ---------------------------------------------------- 183
    Tempseas data created ------------------------------------------------------------------ 188
    aggregating temp_all_hot_cold data ----------------------------------------------------- 198

Load EPA & NPN Data
    EPA data created ----------------------------------------------------------------------- 203
    EPA data manipulation ------------------------------------------------------------------ 205

Preliminary Plots and Visualizations
    Line plot grid location bloom day vs. year by location --------------------------------- 241
    Line plot superimposed location bloom day vs. year ------------------------------------- 252
    Scatterplot of peak bloom date vs. average tmax for March/Jan. ------------------------- 266
    Line plot of tmax_avg vs year ---------------------------------------------------------- 297
    Step plot of bloom day vs. sunlight for Washington D.C. -------------------------------- 306
    Statistic summaries for sunlight data -------------------------------------------------- 314
    Step plot of bloom day vs. tmax_avg_2/3 for Feb. & March by location ------------------- 327
    Histogram of average temp grouped by location and seasons ------------------------------ 345
    QQ plot of daily average temp. by location and season ---------------------------------- 357

EPA Data Exploration 
    Principal Component Analysis  
          for Washington D.C. -------------------------------------------------------------- 372
          for Kyoto ------------------------------------------------------------------------ 389
          for Liestal ---------------------------------------------------------------------- 404
    Linear Regression Analysis 
          Testing Models ------------------------------------------------------------------- 413
          Scatterplot of predicated vs. actual bloom day ----------------------------------- 427
    Time Series Analysis
          Estimating Linear Trends --------------------------------------------------------- 438
          Plots ---------------------------------------------------------------------------- 458
          Scatterplot on lagged variable --------------------------------------------------- 471

General Multiple Linear Regression Model for Hot and Cold Covariates
    Fitting Regression --------------------------------------------------------------------- 489
    Model Statistic Summary ---------------------------------------------------------------- 482
    Model Performance: Calculating Absolute Difference ------------------------------------- 505
    Scatterplot % Line Plot of Predicted vs. Actual ---------------------------------------- 520

OLS Model
    Fitting linear regression: Predicting Bloom day using Year by Location ----------------- 531
           Model Performance: Calculating Absolute Difference ------------------------------ 576
    Fitting linear regression: Predicting Bloom day by Spring & Winter --------------------- 587
          Extrapolate average seasonal maximum temperature --------------------------------- 597
          Predicting Bloom Day ------------------------------------------------------------- 619
          Model Performance: Calculating Mean Absolute Difference -------------------------- 646
    Comparing OLS Model -------------------------------------------------------------------- 659

Building Random Forest Model
    Combining all data: df_final & hot/cold covarites -------------------------------------- 674
    Create training and testing datasets: with all covarites ------------------------------- 677
          Impute Missing Data  ------------------------------------------------------------- 685
    Perform randomForest  ------------------------------------------------------------------ 707
          Model Performance: Calculating Mean Absolute Difference -------------------------- 771

Building Gradient Boosted Model
    Create training and testing datasets: excluding sunlight data -------------------------- 791
          Impute Missing Data -------------------------------------------------------------- 799
    Boosting ------------------------------------------------------------------------------- 807
          Model Performance: Calculating Mean Absolute Difference -------------------------- 813
          2011 Prediction Results ---------------------------------------------------------- 823
    Create training and testing datasets: excluding snow ----------------------------------  840
          Impute Missing Data -------------------------------------------------------------- 850
    Gradient Boosted Model Ran ------------------------------------------------------------- 856
          Prediction ----------------------------------------------------------------------- 859
          Model Performance: Calculating Mean Absolute Difference -------------------------- 884

FINAl Model:
Gradient Boosting With Separate Model For D.C
    Create training and testing datasets: excluding cold covarites ------------------------- 911
          Impute Missing Data -------------------------------------------------------------- 922
    Gradient Boosted Model Ran ------------------------------------------------------------- 929
          Prediction------------------------------------------------------------------------ 932
    Create training and testing datasets for Washington D.C. ------------------------------- 947
          Impute Missing Data -------------------------------------------------------------- 955
    Gradient Boosted Model for Just Washington D.C. Ran ------------------------------------ 961
          Prediction ----------------------------------------------------------------------- 964
          Model Performance: Calculating Mean Absolute Difference -------------------------- 977
    Test Errors using different covariates ------------------------------------------------- 991
    GAM Model Testing --------------------------------------------------------------------- 1016

Forecasting for Average Temperature, Hot and Cold Covariates 
    Transform temp_exp data by summarise average temp ------------------------------------- 1030
    QQ plot for average temp. by location ------------------------------------------------- 1036
    Graphs looking at relationship between three variables
          Scatterplot of hot vs. average temp by location --------------------------------- 1060
          Juxtaposed loess smooth average temp & hot against bloom day -------------------- 1076
    Predicting Hot Covariate
          Predicting average temp --------------------------------------------------------- 1103
          Predicting hot given the average temperature ------------------------------------ 1115
          Plot predicted vs. actual for average Temperature ------------------------------- 1128
          Using predicted average temp to predict hot covariate --------------------------- 1143
          Comparing predicted vs. actual average temp. ------------------------------------ 1156
                Scatterplot: actual & predicted hot using the pred avg. temp  --------------1162
          Using the actual avg. temp. to compare to the actual & predicted hot ------------ 1172
                Scatter plot: actual hot & predicted hot using actual average temp. ------- 1175
          Line plot of all three hot variables by it's methods ---------------------------- 1184
    Forecasting Cold Variable ------------------------------------------------------------- 1123
    Combining Predicted and Full Data ----------------------------------------------------- 1228
          Predicting average temp. for 2022:2032 ------------------------------------------ 1242
          Predicting hot for 2022:2032 ---------------------------------------------------- 1246
          Predicting cold for 2022:2032 --------------------------------------------------- 1253
    Combining Full Data Set --------------------------------------------------------------- 1260
          Full Data Set with Hot/Cold Forecasting ----------------------------------------- 1283

MC simulation to Forecast Remaining Covariates
    QQ plot of four covarites ------------------------------------------------------------- 1294
    Forecasting
          df.forecasting data created ----------------------------------------------------- 1361
          Regress covarites --------------------------------------------------------------- 1391
          Generate predicted bloom doy distribution by year and location ------------------ 1425

Generate predicted bloom date for 2022
    Predictions 2022 data created --------------------------------------------------------- 1485
    Output data created ------------------------------------------------------------------- 1491
     
    
    

IMPORTANT Notes/ Questions!***
Change line count!!
MD - file doesn't look good in GitHub but does look okay in RMD and plain text file if need be.!!! If open project then Readme file looks nice. 
R notebook bad idea!

- Can we just copy and paste from given README file for the three locaitons?

