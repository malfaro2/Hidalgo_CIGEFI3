# Trends in Central America: A Spatial Correlation Analysis

This is the code for [paper reference](). Originally written by [Marcela Alfaro](https://github.com/malfaro2), data curated by [Natali Mora]()

See Shiny App in here: https://malfaro18.shinyapps.io/mapas/

Contents:

<table class="table table-bordered table-hover table-condensed">
<thead><tr><th title="Field #1">Index Variable</th>
<th title="Field #2">Time Domain</th>
<th title="Field #3">Data</th>
<th title="Field #4">Descriptive</th>
<th title="Field #5">Tests (global and local)</th>
</tr></thead>
<tbody><tr>
<td>Precipitation</td>
<td>Monthly</td>
<td>0.read_data_month.R</td>
<td>1.descriptive_month_prec.R</td>
<td>2.tests_month.R</td>
</tr>
<tr>
<td>Precipitation</td>
<td>Yearly</td>
<td>0.read_data_year.R</td>
<td>1.descriptive_year_prec.R</td>
<td>2.tests_year.R</td>
</tr>
<tr>
<td>Temperature</td>
<td>Monthly</td>
<td>0.read_data_month.R</td>
<td>1.descriptive_month_temp.R</td>
<td>2.tests_month.R</td>
</tr>
<tr>
<td>Temperature</td>
<td>Yearly</td>
<td>0.read_data_year.R</td>
<td>1.descriptive_year_temp.R</td>
<td>2.tests_year.R</td>
</tr>
</tbody></table>

[Data](datos_original): two sets of indices calculated over the Central American Region. 

  * Precipitation:  CDD (consecutive dry days), CWD (consecutive wet days), PRCPTOT (annual total PRCP in wet days in mm), R10mm (number of heavy precipitation days), R20mm (number of very heavy precipitation days), R95p (very wet days in mm), R99p (extremely wet days in mm), RX1day (max 1-day precipitation amount in mm), RX5day (max 5-day precipitation amount in mm), SDII (simple daily intensity index in mm/day). 174 stations, 32 years (1979-2010), 10 indices.
  
  * Temperature: CSDI (annual count of days with at least 6 consecutive days when TN < 10th percentile, 0s for all locations and years), DTR (Daily temperature range), TN10p (Percentage of days when TN < 10th percentile), TN90p (Percentage of days when TN > 90th percentile), TNn (Minimum value of daily minimum temperature), TNx (Minimum value of daily maximum temperature), TX10p (Percentage of days when TX < 10th percentile), TX90p (Percentage of days when TN > 90th percentile), TXn (Minimum value of daily maximum temperature), TXx (Maximum value of daily maximum temperature), WSDI (annual count of days with at least 6 consecutive days when TX > 90th percentile, 0s for all locations and years). 38 stations, 35 years (1970-2004), 11 indices

[Results](report.html)

Cite:
If you find this code useful in your research, please, consider citing our paper: 
