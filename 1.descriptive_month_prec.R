## Data, functions and packages
source(file="0.packages.R")
source(file="functions/BPfunc_month.R")
source(file="functions/get_plots.R")
source(file="functions/heatmaps.R")
load(file="data_prec_month.Rdata")

# Draw plots and calculate descriptive stats

N <- 174 # number of stations.
estaciones <- tibble("lat" = latlonprec[[1]][,1],
                        "lon" = (latlonprec[[1]][,2])*-1,
                        "station" = c(1:N))
map1 <- rnaturalearth::ne_states(
  country = c("guatemala", "honduras", 
              "el salvador", "panama", 
              "nicaragua", "costa rica", 
              "belize", "mexico", 
              "colombia"), returnclass = "sf")

names(datos_prec)

dat <- datos_prec %>% 
  group_by(year,month,station) %>% 
  left_join(estaciones, by=c("station")) %>% 
  group_by(station) %>% 
  mutate(CDD.m = mean(CDD,na.rm=TRUE),
         CWD.m = mean(CWD,na.rm=TRUE),
         PRCTOT.m = mean(PRCTOT,na.rm=TRUE),
         R10mm.m = mean(R10mm,na.rm=TRUE),
         R20mm.m = mean(R20mm,na.rm=TRUE),
         R95p.m = mean(R95p,na.rm=TRUE),
         R99p.m = mean(R99p,na.rm=TRUE),
         RX1day.m = mean(RX1day,na.rm=TRUE),
         RX5day.m = mean(RX5day,na.rm=TRUE),
         SDII.m = mean(SDII,na.rm=TRUE)) %>% 
  ungroup() %>% 
  mutate(CDD.c = CDD - CDD.m,
         CWD.c = CWD - CWD.m,
         PRCTOT.c = PRCTOT - PRCTOT.m,
         R10mm.c = R10mm - R10mm.m,
         R20mm.c = R20mm - R20mm.m,
         R95p.c = R95p - R95p.m,
         R99p.c = R99p - R99p.m,
         RX1day.c = RX1day - RX1day.m,
         RX5day.c = RX5day - RX5day.m,
         SDII.c = SDII - SDII.m,
         time = as.yearmon(paste0(year+1978,"-",month)) )

## Variable Description - Boxplots

units <- c("NA")
plot_list <- colnames(dat)[4:13] %>% 
  map( ~ BPfunc_month(.x, units))
plot_list[[4]]

jpeg("maps/desc_prec_month1.jpg", width = 1200, height = 750)
(plot_list[[1]] | plot_list[[2]]) /
  (plot_list[[3]] | plot_list[[4]]) 
dev.off()
jpeg("maps/desc_prec_month2.jpg", width = 1200, height = 750)
(plot_list[[5]] | plot_list[[6]]) /
  (plot_list[[7]] | plot_list[[8]])
dev.off()

jpeg("maps/desc_prec_month3.jpg", width = 1200, height = 325)
(plot_list[[9]] | plot_list[[10]]) 
dev.off()

heatmap_list <- colnames(dat)[4:13] %>% 
  map( ~ heatmap_function(.x))
heatmap_list[[4]]

## R99p ARE mostly ALL ZEROES. SDII has infinite values and NAs
dat <- dat %>% 
  dplyr::select(-starts_with("R99p"), -starts_with("SDII"))

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
