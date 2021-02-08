## Data, functions and packages
source(file="0.packages.R")
source(file="functions/BPfunc_month.R")
source(file="functions/get_plots.R")
source(file="functions/heatmaps.R")
load(file="data_temp_month.Rdata")

# Draw plots and calculate descriptive stats

N <- 38 # number of stations.
estaciones <- tibble("lat" = latlontemp[[1]][,1],
                        "lon" = latlontemp[[1]][,2],
                        "station" = c(1:N))
map1 <- rnaturalearth::ne_states(
  country = c("guatemala", "honduras", 
              "el salvador", "panama", 
              "nicaragua", "costa rica", 
              "belize", "mexico", 
              "colombia"), returnclass = "sf")

names(datos_temp)

dat <- datos_temp %>% 
  group_by(year,month,station) %>% 
  left_join(estaciones, by=c("station")) %>% 
  group_by(station) %>% 
  mutate(CSDI.m = mean(CSDI,na.rm=TRUE),
         DTR.m = mean(DTR,na.rm=TRUE),
         TN10p.m = mean(TN10p,na.rm=TRUE),
         TN90p.m = mean(TN90p,na.rm=TRUE),
         TNn.m = mean(TNn,na.rm=TRUE),
         TNx.m = mean(TNx,na.rm=TRUE),
         TX10p.m = mean(TX10p,na.rm=TRUE),
         TX90p.m = mean(TX90p,na.rm=TRUE),
         TXn.m = mean(TXn,na.rm=TRUE),
         TXx.m = mean(TXx,na.rm=TRUE)) %>% 
  ungroup() %>% 
  mutate(CSDI.c = CSDI - CSDI.m,
         DTR.c = DTR - DTR.m,
         TN10p.c = TN10p - TN10p.m,
         TN90p.c = TN90p - TN90p.m,
         TNn.c = TNn - TNn.m,
         TNx.c = TNx - TNx.m,
         TX10p.c =  TX10p - TX10p.m,
         TX90p.c =  TX90p - TX90p.m,
         TXn.c = TXn - TXn.m,
         TXx.c = TXx - TXx.m,
         time = as.yearmon(paste0(year+1978,"-",month)) )

## Variable Description - Boxplots

units <- c("NA")
plot_list <- colnames(dat)[4:13] %>% 
  map( ~ BPfunc_month(.x, units))

jpeg("maps/desc_temp_month1.jpg", width = 1200, height = 750)
  (plot_list[[1]] | plot_list[[2]]) /
  (plot_list[[3]] | plot_list[[4]]) 
dev.off()
jpeg("maps/desc_temp_month2.jpg", width = 1200, height = 750)
  (plot_list[[5]] | plot_list[[6]]) /
  (plot_list[[7]] | plot_list[[8]])
dev.off()

jpeg("maps/desc_temp_month3.jpg", width = 1200, height = 325)
(plot_list[[9]] | plot_list[[10]]) 
dev.off()

heatmap_list <- colnames(dat)[4:13] %>% 
  map( ~ heatmap_function(.x))
heatmap_list[[4]]

## CSDI ARE mostly ALL ZEROES. TXx has an outlier
dat <- dat %>% dplyr::select(-starts_with("CSDI"))

## Trends - Only calculates trends and correlation

cMKt <- function(var){as.numeric(MannKendall(ts(var))$tau)}
cMKs <- function(var){as.numeric(MannKendall(ts(var))$S)}
cMKv <- function(var){as.numeric(MannKendall(ts(var))$varS)}
pMK  <- function(var){as.numeric(MannKendall(ts(var))$sl)}

summary(dat) # transform into station, year, CDD.c ... SDII.c
trends <- dat %>% 
  dplyr::select(station, time, ends_with(".c")) %>% 
  pivot_longer(cols= ends_with(".c"),
               names_to = "variable", 
               values_to = "value") %>% 
  arrange(time) %>% 
  group_by(station, variable) %>% 
  summarize(tauMK = cMKt(value),
            SMK = cMKs(value),
            varSMK = cMKv(value),
            pMK = pMK(value),
            Z = sign(SMK)*(abs(SMK)-1)/sqrt(varSMK),
            pZ = 2*(1-pnorm(abs(Z))),
            n = length(value),
            r = cor(value[-n],value[-1]))

## Describe trends:

all <- unique(trends$variable);all
i<-1
get_plots(i)
summary(trends %>% filter(variable==all[i]))
trends %>% group_by(variable) %>% summarize(meanZ=mean(Z))
