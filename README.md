# Trends in Central America: A Spatial Correlation Analysis

This is the code for [paper reference](). Originally written by [Marcela Alfaro](https://github.com/malfaro2), data curated by [Natali Mora]()


Contents:

[Data](datos_original): two sets of indices calculated over the Central American Region. 

  * Set 1:  CDD (consecutive dry days), CWD (consecutive wet days), PRCPTOT (annual total PRCP in wet days in mm), R10mm (number of heavy precipitation days), R20mm (number of very heavy precipitation days), R95p (very wet days in mm), R99p (extremely wet days in mm), RX1day (max 1-day precipitation amount in mm), RX5day (max 5-day precipitation amount in mm), SDII (simple daily intensity index in mm/day). 174 stations, 32 years (1979-2010), 10 indices
  
  * Set 2:  CSDI (0s for all locations and years),  DTR, TN10p, TN90p, TNn, TNx, TX10p, TX90p, TXn, TXx, WSDI (0s for all locations and years). 46 stations, 35 years (1970-2004), 11 indices

Code to read the data: [Set 1](0.read_data.R), [Set 2](0.read_data2.R)

Descriptives and Trend Analysis: [Set 1](1.descriptive.R), [Set 2](1.descriptive2.R)

Main Results: 

## Are Global Trends for each index significantly increasing?

Set 1: Precipitation (index per year)

|variable  | maxcMK|      Li|      Ls|sign  |
|:---------|------:|-------:|-------:|:-----|
|CDD.c     |    209| 102.000| 199.050|TRUE  |
|CWD.c     |    183| 107.475| 210.100|FALSE |
|PRCPTOT.c |    250|  99.900| 211.150|TRUE  |
|R10mm.c   |    217|  95.700| 213.775|TRUE  |
|R20mm.c   |    236| 109.425| 218.000|TRUE  |
|R95p.c    |    163| 119.425| 198.575|FALSE |
|R99p.c    |    117|  74.475| 161.725|FALSE |
|RX1day.c  |    218| 114.950| 219.775|FALSE |
|RX5day.c  |    211| 108.000| 202.100|TRUE  |
|SDII.c    |    274| 104.000| 214.100|TRUE  |

Set 1: Precipitation (index per month)

|variable | maxcMK|       Li|       Ls|sign |
|:--------|------:|--------:|--------:|:----|
|CDD.c    |  12709| 1132.000| 2923.450|TRUE |
|CWD.c    |  34592|  817.775| 2535.175|TRUE |
|PRCTOT.c |  33747|  612.000| 2510.250|TRUE |
|R10mm.c  |  33834|  591.700| 2541.325|TRUE |
|R20mm.c  |  32953|  612.475| 2594.525|TRUE |
|R95p.c   |  20403|  939.825| 2725.025|TRUE |
|RX1day.c |  28593|  761.400| 2616.150|TRUE |
|RX5day.c |  32341|  714.925| 2720.650|TRUE |
 
Set 2: Temperature (index per year)

|variable | maxcMK|     Li|      Ls|sign  |
|:--------|------:|------:|-------:|:-----|
|DTR.c    |    391| 75.375| 226.675|TRUE  |
|TN10p.c  |    228| 73.000| 206.300|TRUE  |
|TN90p.c  |    283| 60.950| 227.350|TRUE  |
|TNn.c    |    295| 42.900| 210.150|TRUE  |
|TNx.c    |    270| 77.325| 221.200|TRUE  |
|TX10p.c  |    126| 46.475| 221.575|FALSE |
|TX90p.c  |    264| 68.375| 224.050|TRUE  |
|TXn.c    |    230| 56.000| 205.250|TRUE  |
|TXx.c    |    343| 53.000| 225.050|TRUE  |

Set 2: Temperature (index per month)

|variable | maxcMK|     Li|      Ls|sign  |
|:--------|------:|------:|-------:|:-----|
|CSDI.c   |   1753| 34.000|  86.050|TRUE  |
|DTR.c    |  -8384| 36.325| 224.775|FALSE |
|TN10p.c  |   1784| 49.375| 214.525|TRUE  |
|TN90p.c  |   2794| 32.500| 218.050|TRUE  |
|TNn.c    |  22230| 41.850| 216.525|TRUE  |
|TNx.c    |  13935| 62.850| 241.525|TRUE  |
|TX10p.c  |   3699| 34.175| 221.625|TRUE  |
|TX90p.c  |   5602| 57.000| 211.000|TRUE  |
|TXn.c    |  13748| 47.325| 230.875|TRUE  |
|TXx.c    |  15158| 69.325| 223.400|TRUE  |

## Are Local Trends for each index significantly increasing? 

(maps)

Cite:
If you find this code useful in your research, please, consider citing our paper: 
