# Trends in Central America: A Spatial Correlation Analysis

This is the code for [paper reference](). Originally written by [Marcela Alfaro ](https://github.com/malfaro2).


Contents:

[Data](datos_original): two sets of indices calculated over the Central American Region. 

  * Set 1:  CDD (consecutive dry days), CWD (consecutive wet days), PRCPTOT (annual total PRCP in wet days in mm), R10mm (number of heavy precipitation days), R20mm (number of very heavy precipitation days), R95p (very wet days in mm), R99p (extremely wet days in mm), RX1day (max 1-day precipitation amount in mm), RX5day (max 5-day precipitation amount in mm), SDII (simple daily intensity index in mm/day). 174 stations, 32 years (1979-2010), 10 indices
  
  * Set 2:  CSDI,  DTR, TN10p, TN90p, TNn, TNx, TX10p, TX90p, TXn, TXx, WSDI. 46 stations, 35 years (1970-2004), 11 indices

Code to read the data: [Set 1](0.read_data.R), [Set 2](0.read_data2.R)

Descriptives and Trend Analysis: [Set 1](1.descriptive.R), [Set 2](1.descriptive2.R)


Cite:
If you find this code useful in your research, please, consider citing our paper: 
