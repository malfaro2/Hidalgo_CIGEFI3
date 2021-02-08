
###################################
####  PRECIPITATION MONTHLY  ######
###################################

## Data and packages
rm(list=ls())
source(file="0.packages.R")
source(file="functions/get_maps.R")
load("data_proc/tab_local_month_prec.Rdata")
load(file="data_proc/data_prec_month.Rdata")
rm(datos_prec)
load(file="data_proc/tab_trends_month_prec.Rdata")
load(file="maps/plot_list_month_prec.Rdata")
load(file="maps/heatmap_list_month_prec.Rdata")

allin <- function(i){
datos <- data.frame(tab_local_month_prec[[i]], latlonprec$latlon) 
names(datos) <- c("S", "P_025","P_975", "lat", "lon")
datos <- tibble(datos)
get_maps(datos,i)
}

map_list <- lapply(1:length(tab_local_month_prec),allin)

tab <- tab_trends_month_prec %>% 
  mutate(sign = ifelse(meanZ < 0, "-", "+")) %>% 
  tableGrob(theme = ttheme_minimal(), rows = NULL)
grid.arrange(tab)

### Remove index 7 and 10
### R99p ARE mostly ALL ZEROES. SDII has infinite.

i<-10
jpeg(paste0("maps/prec_month_index",i,".jpg"), width = 1200, height = 650)
 # layout <- (map_list[[i]] + tab) / plot_list_month_prec[[i]]
 #j <- i-1
 # layout <- (map_list[[j]] + tab) / plot_list_month_prec[[i]]
  layout <- (heatmap_list_month_prec[[i]]) / plot_list_month_prec[[i]]
  layout + plot_annotation(
    #title = paste('Results for Monthly Index',all[[j]]),
    #title = paste("Results for Monthly Index R99p"),
    title = paste("Results for Monthly Index SDII"),
    subtitle = 'Time Series, Map with Local Significance and Global Significance',
    caption = 'Data Source: CLIMDEX')
dev.off()

###################################
####  PRECIPITATION yearLY  ######
###################################

## Data and packages
rm(list=ls())
source(file="0.packages.R")
source(file="functions/get_maps.R")
load("data_proc/tab_local_year_prec.Rdata")
load(file="data_proc/data_prec_year.Rdata")
rm(datos)
load(file="data_proc/tab_trends_year_prec.Rdata")
load(file="maps/plot_list_year_prec.Rdata")

allin <- function(i){
  datos <- data.frame(tab_local_year_prec[[i]], locations) 
  names(datos) <- c("S", "P_025","P_975", "lat", "lon")
  datos <- tibble(datos)
  get_maps(datos,i)
}

map_list <- lapply(1:length(tab_local_year_prec),allin)

tab <- tab_trends_year_prec %>% 
  mutate(sign = ifelse(meanZ < 0, "-", "+")) %>% 
  tableGrob(theme = ttheme_minimal(), rows = NULL)
grid.arrange(tab)

i<-10
jpeg(paste0("maps/prec_year_index",i,".jpg"), width = 1200, height = 650)
layout <- (map_list[[i]] + tab) / plot_list_year_prec[[i]]
layout + plot_annotation(
  title = paste('Results for Yearly Index',all[[i]]),
  subtitle = 'Time Series, Map with Local Significance and Global Significance',
  caption = 'Data Source: CLIMDEX')
dev.off()

###################################
####  TEMPERATURE MONTHLY  ######
###################################
rm(list=ls())
## Data and packages
source(file="0.packages.R")
source(file="functions/get_maps.R")
load("data_proc/tab_local_month_temp.Rdata")
load(file="data_proc/data_temp_month.Rdata")
rm(datos_temp)
load(file="data_proc/tab_trends_month_temp.Rdata")
load(file="maps/plot_list_month_temp.Rdata")
load(file="maps/heatmap_list_month_temp.Rdata")

allin <- function(i){
  datos <- data.frame(tab_local_month_temp[[i]], latlontemp$latlon) 
  names(datos) <- c("S", "P_025","P_975", "lat", "lon")
  datos <- tibble(datos)
  datos$lon <- -datos$lon
  get_maps(datos,i)
}

map_list <- lapply(1:length(tab_local_month_temp),allin)

tab <- tab_trends_month_temp %>% 
  mutate(sign = ifelse(meanZ < 0, "-", "+")) %>% 
  tableGrob(theme = ttheme_minimal(), rows = NULL)
grid.arrange(tab)

### Remove index 1 
## CSDI ARE mostly ALL ZEROES

i<-10
jpeg(paste0("maps/temp_month_index",i,".jpg"), width = 1200, height = 650)
j <- i-1
layout <- (map_list[[j]] + tab) / plot_list_month_temp[[i]]
#layout <- (heatmap_list_month_temp[[i]]) / plot_list_month_temp[[i]]
layout + plot_annotation(
  title = paste('Results for Monthly Index',all[[j]]),
  #title = paste("Results for Monthly Index CSDI"),
  subtitle = 'Time Series, Map with Local Significance and Global Significance',
  caption = 'Data Source: CLIMDEX')
dev.off()

###################################
####  TEMPERATURE YEARLY  ######
###################################
rm(list=ls())
## Data and packages
source(file="0.packages.R")
source(file="functions/get_maps.R")
load("data_proc/tab_local_year_temp.Rdata")
load(file="data_proc/data_temp_year.Rdata")
rm(datos)
load(file="data_proc/tab_trends_year_temp.Rdata")
load(file="maps/plot_list_year_temp.Rdata")

allin <- function(i){
  datos <- data.frame(tab_local_year_temp[[i]], locations) 
  names(datos) <- c("S", "P_025","P_975", "lat", "lon")
  datos <- tibble(datos)
  datos$lon <- -datos$lon
  get_maps(datos,i)
}

map_list <- lapply(1:length(tab_local_year_temp),allin)

tab <- tab_trends_year_temp %>% 
  mutate(sign = ifelse(meanZ < 0, "-", "+")) %>% 
  tableGrob(theme = ttheme_minimal(), rows = NULL)
grid.arrange(tab)

## CSDI, WSDI ARE ALL ZEROES.
## Erase 1, 11

i<-11
jpeg(paste0("maps/temp_year_index",i,".jpg"), width = 1200, height = 650)
j<- i-1
#layout <- (map_list[[j]] + tab) / plot_list_year_temp[[i]]
layout <- plot_list_year_temp[[i]]+tab
layout + plot_annotation(
  #title = paste("Results for Yearly Index CSDI"),
  title = paste("Results for Yearly Index WSDI"),
  #title = paste('Results for Yearly Index',all[[j]]),
  subtitle = 'Time Series, Map with Local Significance and Global Significance',
  caption = 'Data Source: CLIMDEX')
dev.off()
