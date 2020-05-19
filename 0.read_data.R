source("0.packages.R")

dat <- readMat("datos_original/indices_CA_1979_2010.mat")

length(dat)
lapply(dat, dim)

## 174 stations, 32 years, lat, lon, 10 indices

plot(-dat[[11]][,2],dat[[11]][,1])

## Create a tibble with all variables:

datos<- tibble("year" = rep(1:32,174),
               "station"  = rep(1:174,each=32),
 # CDD: consecutive dry days: maximum number of consecutive 
 # days with daily rainfall < 1mm (in days)
                "CDD"  = as.vector(dat[[1]]), 
 # CWD: consecutive wet days: maximum number of consecutive 
 # days with daily rainfall >= 1mm (in days)
                "CWD"  = as.vector(dat[[2]]), 
 # PRCPTOT: annual total PRCP in wet days (RR >= 1mm) (in mm)
                "PRCPTOT"  = as.vector(dat[[3]]), 
 # R10mm: number of heavy precipitation days, 
 # annual count of days when precipitation >= 10mm (in days)
                "R10mm"  = as.vector(dat[[4]]), 
 # R20mm: number of very heavy precipitation days, 
 # annual count of days when precipitation >= 20mm (in days)
                "R20mm"  = as.vector(dat[[5]]), 
 # R95p: very wet days, annual total PRCP 
 # when RR > 95th percentile (in mm)
                "R95p"  = as.vector(dat[[6]]), 
 # R99p: extremely wet days, annual total PRCP 
 # when RR > 99th percentile (in mm)
                "R99p"   = as.vector(dat[[7]]), 
 # RX1day: max 1-day precipitation amount, 
 # annual maximum 1-day precipitation (in mm)
                "RX1day"  = as.vector(dat[[8]]), 
 # RX5day: max 5-day precipitation amount, 
 #  # annual maximum 5-day precipitation (in mm)
                "RX5day"  = as.vector(dat[[9]]), 
 # SDII: simple daily intensity index, annual total 
 # precipitation divided by the number of wet days (defined as
 # precipitation >= 1.0mm) in the year (in mm/day)
                "SDII"  = as.vector(dat[[10]])) 

locations <- dat[[11]]

save(datos,locations, file="data.Rdata")
