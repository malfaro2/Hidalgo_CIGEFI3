# Trends in Central America: A Spatial Correlation Analysis

This is the code for [paper reference](). Originally written by [Marcela Alfaro](https://github.com/malfaro2), data curated by [Natali Mora]()


Contents:

[Data](datos_original): two sets of indices calculated over the Central American Region. 

  * Set 1:  CDD (consecutive dry days), CWD (consecutive wet days), PRCPTOT (annual total PRCP in wet days in mm), R10mm (number of heavy precipitation days), R20mm (number of very heavy precipitation days), R95p (very wet days in mm), R99p (extremely wet days in mm), RX1day (max 1-day precipitation amount in mm), RX5day (max 5-day precipitation amount in mm), SDII (simple daily intensity index in mm/day). 174 stations, 32 years (1979-2010), 10 indices
  
  * Set 2:  CSDI (0s for all locations and years),  DTR, TN10p, TN90p, TNn, TNx, TX10p, TX90p, TXn, TXx, WSDI (0s for all locations and years). 46 stations, 35 years (1970-2004), 11 indices

Code to read the data: [Set 1](0.read_data.R), [Set 2](0.read_data2.R)

Descriptives and Trend Analysis: [Set 1](1.descriptive.R), [Set 2](1.descriptive2.R)

Main Results: Are Global Trends for each index significantly increasing?

Set 1:

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
 
Set 2:

|variable | maxcMK|     Li|      Ls|sign  |
|:--------|------:|------:|-------:|:-----|
|DTR.c    |    397| 83.475| 235.100|TRUE  |
|TN10p.c  |    222| 70.175| 219.875|TRUE  |
|TN90p.c  |    239| 60.800| 218.100|TRUE  |
|TNn.c    |    239| 62.750| 218.625|TRUE  |
|TNx.c    |    329| 80.475| 216.050|TRUE  |
|TX10p.c  |    126| 64.475| 211.125|FALSE |
|TX90p.c  |    264| 73.650| 224.050|TRUE  |
|TXn.c    |    221| 71.475| 218.725|TRUE  |
|TXx.c    |    343| 58.225| 236.200|TRUE  |


Cite:
If you find this code useful in your research, please, consider citing our paper: 
