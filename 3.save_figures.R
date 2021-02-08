
###################################
####  PRECIPITATION MONTHLY  ######
###################################

## Data and packages
rm(list=ls())
source(file="0.packages.R")
source(file="functions/get_maps.R")
load("tab_local_month_prec.Rdata")
load(file="data_prec_month.Rdata")

allin <- function(i){
datos <- data.frame(tab_local_month_prec[[i]], latlonprec$latlon) 
names(datos) <- c("S", "P_025","P_975", "lat", "lon")
datos <- tibble(datos)
get_maps(datos,i)
}

map_list <- lapply(1:length(tab_local_month_prec),allin)

jpeg("maps/prec_month1.jpg", width = 1200, height = 750)
(map_list[[1]] | map_list[[2]]) /
  (map_list[[3]] | map_list[[4]]) 
dev.off()
jpeg("maps/prec_month2.jpg", width = 1200, height = 750)
(map_list[[5]] | map_list[[6]]) /
  (map_list[[7]] | map_list[[8]])
dev.off()

###################################
####  TEMPERATURE MONTHLY  ######
###################################
rm(list=ls())
## Data and packages
source(file="0.packages.R")
source(file="functions/get_maps.R")
load("tab_local_month_temp.Rdata")
load(file="data_temp_month.Rdata")

allin <- function(i){
  datos <- data.frame(tab_local_month_temp[[i]], latlontemp$latlon) 
  names(datos) <- c("S", "P_025","P_975", "lat", "lon")
  datos <- tibble(datos)
  datos$lon <- -datos$lon
  get_maps(datos,i)
}

map_list <- lapply(1:length(tab_local_month_temp),allin)

jpeg("maps/temp_month1.jpg", width = 1200, height = 750)
(map_list[[1]] | map_list[[2]]) /
  (map_list[[3]] | map_list[[4]]) 
dev.off()
jpeg("maps/temp_month2.jpg", width = 1200, height = 750)
(map_list[[5]] | map_list[[6]]) /
  (map_list[[7]] | map_list[[8]])
dev.off()

###################################
####  PRECIPITATION yearLY  ######
###################################

## Data and packages
rm(list=ls())
source(file="0.packages.R")
source(file="functions/get_maps.R")
load("tab_local_year_prec.Rdata")
load(file="data_prec_year.Rdata")

allin <- function(i){
  datos <- data.frame(tab_local_year_prec[[i]], locations) 
  names(datos) <- c("S", "P_025","P_975", "lat", "lon")
  datos <- tibble(datos)
  get_maps(datos,i)
}

map_list <- lapply(1:length(tab_local_year_prec),allin)

jpeg("maps/prec_year1.jpg", width = 1200, height = 750)
(map_list[[1]] | map_list[[2]]) /
  (map_list[[3]] | map_list[[4]]) 
dev.off()
jpeg("maps/prec_year2.jpg", width = 1200, height = 750)
(map_list[[5]] | map_list[[6]]) /
  (map_list[[7]] | map_list[[8]])
dev.off()

###################################
####  TEMPERATURE YEARLY  ######
###################################
rm(list=ls())
## Data and packages
source(file="0.packages.R")
source(file="functions/get_maps.R")
load("tab_local_year_temp.Rdata")
load(file="data_temp_year.Rdata")

allin <- function(i){
  datos <- data.frame(tab_local_year_temp[[i]], locations) 
  names(datos) <- c("S", "P_025","P_975", "lat", "lon")
  datos <- tibble(datos)
  datos$lon <- -datos$lon
  get_maps(datos,i)
}

map_list <- lapply(1:length(tab_local_year_temp),allin)

jpeg("maps/temp_year1.jpg", width = 1200, height = 750)
(map_list[[1]] | map_list[[2]]) /
  (map_list[[3]] | map_list[[4]]) 
dev.off()
jpeg("maps/temp_year2.jpg", width = 1200, height = 750)
(map_list[[5]] | map_list[[6]]) /
  (map_list[[7]] | map_list[[8]])
dev.off()



