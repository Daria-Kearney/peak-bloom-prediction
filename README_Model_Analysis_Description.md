# Model Analysis Description for Peak Bloom Prediction #

## Objective of README file ##
Assist in the navigation through the main R script named "Peak Bloom Prediction"

# **Table of Contents** #                                                                Line Number
Load Packages and Data
    Library ----------------------------------------------------------------------------------- 5
    Load cherry data ------------------------------------------------------------------------- 21

RNOAA Data Wrangling and Avg Daily Sunlight (KJ/m^2)
    Get_temperature function ----------------------------------------------------------------- 33
    Historical_temperature data created ------------------------------------------------------ 46
    Get_climate function --------------------------------------------------------------------- 52
    Historic_data created -------------------------------------------------------------------- 78
    Load sunlight data ----------------------------------------------------------------------- 95
    Sunlight data cleaning ------------------------------------------------------------------- 98
    Df_final data created ------------------------------------------------------------------- 105

RNOAA Data Wrangling for Hot and Cold Covariates 
    Get_tmax function ----------------------------------------------------------------------- 118
    Days_max data created ------------------------------------------------------------------- 126
    Get_tmin function ----------------------------------------------------------------------- 133
    Days_min data created ------------------------------------------------------------------- 141
    Tempall data created -------------------------------------------------------------------- 148
    Binary hot/ cold variables created ------------------------------------------------------ 154
    Temp_all_hot_cold data created ---------------------------------------------------------- 160
    Temp_all_hot_cold data cleaning --------------------------------------------------------- 164
    Temp_exp data created ------------------------------------------------------------------- 176
    Temp_all_hot_cold data manipulation ----------------------------------------------------- 182
    Tempseas data created ------------------------------------------------------------------- 187
    aggregating temp_all_hot_cold data ------------------------------------------------------ 197

Load EPA & NPN Data
    EPA data created ------------------------------------------------------------------------ 203
    EPA data manipulation ------------------------------------------------------------------- 205

Preliminary Plots and Visualizations
    Line plot grid location bloom day vs. year by location ---------------------------------- 241
    Line plot superimposed location bloom day vs. year -------------------------------------- 252
    Scatterplot of peak bloom date vs. average tmax for March/Jan. -------------------------- 266
    Line plot of tmax_avg vs year ----------------------------------------------------------- 298
    Step plot of bloom day vs. sunlight for Washington D.C. --------------------------------- 308
    Statistic summaries for sunlight data --------------------------------------------------- 316
    Step plot of bloom day vs. tmax_avg_2/3 for Feb. & March by location -------------------- 330
    Histogram of average temp grouped by location and seasons ------------------------------- 351
    QQ plot of daily average temp. by location and season ----------------------------------- 368

EPA Data Exploration 
    Principal Component Analysis  
          for Washington D.C. --------------------------------------------------------------- 389
          for Kyoto ------------------------------------------------------------------------- 403
          for Liestal ----------------------------------------------------------------------- 418
    Linear Regression Analysis 
          Testing Models -------------------------------------------------------------------- 428
          Scatterplot of predicated vs. actual bloom day ------------------------------------ 441
    Time Series Analysis
          Estimating Linear Trends ---------------------------------------------------------- 453
          Plots ----------------------------------------------------------------------------- 471
          Scatterplot on lagged variable ---------------------------------------------------- 486

General Multiple Linear Regression Model for Hot and Cold Covariates
    Fitting Regression ---------------------------------------------------------------------- 504
    Model Statistic Summary ----------------------------------------------------------------- 507
    Model Performance: Calculating Absolute Difference -------------------------------------- 519
    Scatterplot & Line Plot of Predicted vs. Actual ----------------------------------------- 535

OLS Model
    Fitting linear regression: Predicting Bloom day using Year by Location ------------------ 549
           Model Performance: Calculating Absolute Difference ------------------------------- 603
    Fitting linear regression: Predicting Bloom day by Spring & Winter ---------------------- 605
          Extrapolate average seasonal maximum temperature ---------------------------------- 615
          Predicting Bloom Day -------------------------------------------------------------- 619
          Model Performance: Calculating Mean Absolute Difference --------------------------- 675
    Comparing OLS Model --------------------------------------------------------------------- 677

Building Random Forest Model
    Combining all data: df_final & hot/cold covarites --------------------------------------- 693
    Create training and testing datasets: with all covarites -------------------------------- 695
          Impute Missing Data  -------------------------------------------------------------- 703
    Perform randomForest  ------------------------------------------------------------------- 707
          Model Performance: Calculating Mean Absolute Difference --------------------------- 717
    Create training testing set with D.C ---------------------------------------------------- 760
    Perform randomForest with D.C ----------------------------------------------------------- 770
          Model Performance: Calculating Mean Absolute Difference --------------------------- 800

Building Gradient Boosted Model
    Create training and testing datasets: excluding sunlight data --------------------------- 809
          Impute Missing Data --------------------------------------------------------------- 817
    Boosting -------------------------------------------------------------------------------- 825
          Model Performance: Calculating Mean Absolute Difference --------------------------- 834
          2011 Prediction Results ----------------------------------------------------------- 841
    Create training and testing datasets: excluding snow -----------------------------------  858
          Impute Missing Data --------------------------------------------------------------- 868
    Gradient Boosted Model Ran -------------------------------------------------------------- 874
          Prediction ------------------------------------------------------------------------ 877
          Model Performance: Calculating Mean Absolute Difference --------------------------- 913

FINAl Model:
Gradient Boosting With Separate Model For D.C
    Create training and testing datasets: excluding cold covarites -------------------------- 929
          Impute Missing Data --------------------------------------------------------------- 939
    Gradient Boosted Model Ran -------------------------------------------------------------- 947
          Prediction------------------------------------------------------------------------- 950
    Create training and testing datasets for Washington D.C. -------------------------------- 965
          Impute Missing Data --------------------------------------------------------------- 973
    Gradient Boosted Model for Just Washington D.C. Ran ------------------------------------- 979
          Prediction ------------------------------------------------------------------------ 982
          Model Performance: Calculating Mean Absolute Difference -------------------------- 1006
    Test Errors using different covariates ------------------------------------------------- 1008
    GAM Model Testing ---------------------------------------------------------------------- 1029

Forecasting for Average Temperature, Hot and Cold Covariates 
    Transform temp_exp data by summarise average temp ------------------------------------- 1042
    QQ plot for average temp. by location ------------------------------------------------- 1047
    Graphs looking at relationship between three variables
          Scatterplot of hot vs. average temp by location --------------------------------- 1069
          Juxtaposed loess smooth average temp & hot against bloom day -------------------- 1086
    Predicting Hot Covariate
          Predicting average temp --------------------------------------------------------- 1112
          Predicting hot given the average temperature ------------------------------------ 1124
          Plot predicted vs. actual for average Temperature ------------------------------- 1137
          Using predicted average temp to predict hot covariate --------------------------- 1152
          Comparing predicted vs. actual average temp. ------------------------------------ 1165
                Scatterplot: actual & predicted hot using the pred avg. temp  ------------- 1170
          Using the actual avg. temp. to compare to the actual & predicted hot ------------ 1182
                Scatter plot: actual hot & predicted hot using actual average temp. ------- 1187
          Line plot of all three hot variables by it's methods ---------------------------- 1199
    Forecasting Cold Variable ------------------------------------------------------------- 1237
    Combining Predicted and Full Data ----------------------------------------------------- 1242
          Predicting average temp. for 2022:2032 ------------------------------------------ 1255
          Predicting hot for 2022:2032 ---------------------------------------------------- 1259
          Predicting cold for 2022:2032 --------------------------------------------------- 1265
    Combining Full Data Set --------------------------------------------------------------- 1271
          Full Data Set with Hot/Cold Forecasting ----------------------------------------- 1297

MC simulation to Forecast Remaining Covariates
    QQ plot of four covarites ------------------------------------------------------------- 1306
    Forecasting
          df.forecasting data created ----------------------------------------------------- 1374
          Regress covarites --------------------------------------------------------------- 1404
          Generate predicted bloom doy distribution by year and location ------------------ 1438

Generate predicted bloom date for 2022
    Predictions 2022 data created --------------------------------------------------------- 1482
    Output data created ------------------------------------------------------------------- 1502
