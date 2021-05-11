###################################
####  PRECIPITATION yearLY  ######
###################################

## Data and packages
rm(list=ls())
source(file="0.packages.R")
source(file="functions/get_maps_all.R")
load("data_proc/tab_local_bimonth3_prec.Rdata")
load(file="data_proc/data_prec_bimonth.Rdata")
rm(datos_prec)

allin <- function(i){
  datos <- data.frame(tab_local_bimonth_prec[[i]], latlonprec) 
  names(datos) <- c("S", "P_025","P_975", "lat", "lon")
  datos <- data.frame(datos %>% mutate(signo = case_when(S >= 0 ~ "POSITIVE", S < 0 ~ "NEGATIVE")))
  names(datos) <- c("S", "P_025","P_975", "lat", "lon", "signo")
  datos <- tibble(datos)
  get_maps(datos,i)
  
}

map_list <- lapply(1:length(tab_local_bimonth_prec),allin)

for(i in 1:8){
  #i=4
  tiff(paste0("maps/Prec/MJ/prec_bimonth_index",i,"_MJ",".tif"), width = 2500, height = 1500,res = 300)
  layout <- (map_list[[i]])
  #print(i)
  print(layout)
# + plot_annotation( 
#    title = paste('Results for Monthly Index', all[[i]]),
#    subtitle = 'Map with Local Significance',
#    caption = 'Data Source: CLIMDEX'))
  dev.off()
}

###################################
####  TEMPERATURE YEARLY  ######
###################################
rm(list=ls())
## Data and packages
source(file="0.packages.R")
source(file="functions/get_maps_all.R")
load("data_proc/tab_local_bimonth3_temp.Rdata")
load(file="data_proc/data_temp_bimonth.Rdata")
#write.csv(est_menos_quincenan, file = "rellenar_temp_min_1970-2004.csv")
rm(datos_temp)

allin <- function(i){
  datos <- data.frame(tab_local_bimonth_temp[[i]], latlontemp) 
  names(datos) <- c("S", "P_025","P_975", "lat", "lon")
  datos <- data.frame(datos %>% mutate(signo = case_when(S >= 0 ~ "POSITIVE", S < 0 ~ "NEGATIVE")))
  names(datos) <- c("S", "P_025","P_975", "lat", "lon", "signo")
  datos <- tibble(datos)
  datos$lon <- -datos$lon
  get_maps(datos,i)
}

map_list <- lapply(1:length(tab_local_bimonth_temp),allin)

## CSDI, WSDI ARE ALL ZEROES.
## Erase 1, 11

for(i in 1:5){
  i<-7
  tiff(paste0("maps/Temp_bimonth/MJ/temp_bimonth_index",i,"_MJ",".tif"), width = 2500, height = 1500,res = 300)
   #j<-i-1
  layout <- (map_list[[i]]) 
  print(layout)
dev.off()
}  
