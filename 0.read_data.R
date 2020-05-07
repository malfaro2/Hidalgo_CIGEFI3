source("0.packages.R")

dat <- readMat("datos_original/indices_CA_1979_2010.mat")

length(dat)
lapply(dat, dim)

## 174 stations, 32 years, lat, lon, 10 indices

plot(-dat[[11]][,2],dat[[11]][,1])

## Create a tibble with all variables:

datos<- tibble("year" = rep(1:32,174),
               "station"  = rep(1:174,each=32),
               "CDD"  = as.vector(dat[[1]]),
               "CWD"  = as.vector(dat[[2]]),
               "PRCPTOT"  = as.vector(dat[[3]]),
               "R10mm"  = as.vector(dat[[4]]),
               "R20mm"  = as.vector(dat[[5]]),
               "R95p"  = as.vector(dat[[6]]),
               "R99p"   = as.vector(dat[[7]]),
               "RX1day"  = as.vector(dat[[8]]),
               "RX5day"  = as.vector(dat[[9]]),
               "SDII"  = as.vector(dat[[10]]))

locations <- dat[[11]]

save(datos,locations, file="data.Rdata")
