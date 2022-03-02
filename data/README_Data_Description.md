# Data Set Description for Peak Bloom Prediction #

## Objective of README file ##
Through description of all data set in the main R script named "Peak Bloom Prediction"

# Data Sets in the "data" file
## Cleaned Data
**"sunlight.csv""**

Source: https://wonder.cdc.gov/nasa-insolar.html

68 obs. of 7 variables: 

* location: washingtondc & vancouver
* year - 1979 to 2012
* sunlight_avg_i = measured in (Kj/m^2)
* sunlight _avg_0 - Average sunlight for December 
* sunlight _avg_1- Average sunlight for Jan.
* sunlight _avg_2- Average sunlight for Feb
* sunlight _avg_3 - Average sunlight for March
* sunlight _avg_4- Average sunlight for April

**"EPA.csv"**

Source: https://www.epa.gov/climate-indicators/weather-climate

30 obs. of 13 variables

* year: 1990 to 2019
* Carbon.dioxide- measured parts-per-million (ppm)
* Methane - measured parts-per-million (ppm)
* Nitrous.oxide - measured parts-per-million (ppm)
* HFCs..PFSs..SF6..and.NF3
* Anomaly
* Winter
* Spring
* Summer
* Fall
* Earth.s.surface - Earth surface temperatures measured in temperature anomaly (°F)
* Lower.trosposphere..measured.by.satellite...UAH l
* Lower.trosposphere..measured.by.satellite...RSS

# Data Given
**"kyoto.csv"**
---
**"liestal.csv"**
----
**"washingtondc.csv"**
---
The structure of the cleaned data files is as follows:

* location: washingtondc, liestal, kyoto 
* lat (approximate) latitude of the observation 
* long (approximate) longitude of the observation 
* alt (approximate) altitude of the observation 
* year:
* *bloom_date* date of peak bloom of the cherry trees. The "peak bloom date" may be defined differently for different locations.
* *bloom_doy* days since January 1st of the year until peak bloom. January 1st is `1`.
_Taken from the peak-bloom-prediction main GitHub README file_

**"cherry"**
1062 obs. of 7 variables
* row binded of three csv files: kyoto, liestal & washingtondc

# Data from RNOAA package
**"historic_data"**

1112 obs. of 37 variables

- location: kyoto, washingtondc, vancouver, liestal
- year: 812 (for kyoto), 1950 (for rest of location) to 2022
- t_max_avg_i = Average maximum temperature measured in 1/10 °C
- t_max_avg_0- Average max temperature for December 
- t_max_avg_1- Average max temperature for Jan.
- t_max_avg_2- Average max temperature for Feb.
- t_max_avg_3- Average max temperature for March 
- t_max_avg_4- Average max temperature for April

**"historic_temperatures"**

1018 obs. of 4 variables
- location: kyoto, washingtondc, vancouver, liestal
- year: 1950 to 2022
- season: Winter, Spring, Fall, Summer
- tmax_avg: Average maximum temperature for the given seasons in in 1/10 °C

**"days_max & days_min"**

~90900 obs. 3 variables
- location: kyoto, washingtondc, vancouver, liestal
- date: daily dates from 1950-01-01 to 2021-02-28
- tmax: Daily maximum temperature measured in 1/10 °C
- tmin: Daily minimum temperature measured in 1/10 °C




- t_min_avg_i = Average minimum temperature measured in 1/10 °C
- t_min_avg_0- Average min temperature for December 
- t_min_avg_1- Average min temperature for Jan.
- t_min_avg_2- Average min temperature for Feb.
- t_min_avg_3- Average min temperature for March
- t_min_avg_4- Average min temperature for April

- prcp_avg_i = Average Precipitation in tenths of millimeters (1/10 mm)
- prcp_avg_0- Average precipitation for December 
- prcp_avg_1- Average precipitation for Jan.
- prcp_avg_2- Average precipitation for Feb.
- prcp_avg_3- Average precipitation for March
- prcp_avg_4- Average precipitation for April

- prcp_tot_i = Total Precipitation in tenths of millimeters (mm)
- prcp_tot_0- Total precipitation for December 
- prcp_tot_1- Total precipitation for Jan.
- prcp_tot_2- Total precipitation for Feb.
- prcp_tot_3- Total precipitation for March
- prcp_tot_4- Total precipitation for April

- snwd _avg_i = Average snow fall measured in millimeters (mm)
- snwd _avg_0- Average snow fall for December 
- snwd _avg_1- Average snow fall for Jan.
- snwd _avg_2- Average snow fall for Feb.
- snwd _avg_3- Average snow fall for March
- snwd _avg_4- Average snow fall for April

- snwd _tot_i = Total snow fall measured in millimeters (mm)
- snwd _tot_0- Total snow fall for December 
- snwd _tot_1- Total snow fall for Jan.
- snwd _tot_2- Total snow fall for Feb.
- snwd _tot_3- Total snow fall for March
- snwd _tot_4- Total snow fall for April

- lat: Latatitude
- long: Longitude
- bloom_date- same as described above
- bloom_doy- same as described above
