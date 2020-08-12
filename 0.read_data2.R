source("0.packages.R")

dat <- readMat("datos_original/indices_temp_CA_1970_2004.mat")

length(dat)
lapply(dat, dim)

#names = [CSDI,  DTR, TN10p, TN90p, TNn, TNx, TX10p, TX90p, TXn, TXx, WSDI]
#units = [“ % days”, “ ºC”, “% days”, "% days”, “ ºC”,  “ ºC", “ % days”, “ % days”  “ ºC”, “ ºC”, "% days”];

## 46 stations, 35 years, lat, lon, 11 indices

plot(dat[[12]][,2],dat[[12]][,1])

## Create a tibble with all variables:

datos<- tibble("year" = rep(1:35,38),
               "station"  = rep(1:38,each=35),
"CSDI"  = as.vector(dat[[1]]), 
"DTR"  = as.vector(dat[[2]]), 
"TN10p"  = as.vector(dat[[3]]), 
"TN90p"  = as.vector(dat[[4]]), 
"TNn"  = as.vector(dat[[5]]), 
"TNx"  = as.vector(dat[[6]]), 
"TX10p"   = as.vector(dat[[7]]), 
"TX90p"  = as.vector(dat[[8]]), 
"TXn"  = as.vector(dat[[9]]), 
"TXx"  = as.vector(dat[[10]]), 
"WSDI" = as.vector(dat[[11]]))
locations <- dat[[12]]

save(datos,locations, file="data2.Rdata")
