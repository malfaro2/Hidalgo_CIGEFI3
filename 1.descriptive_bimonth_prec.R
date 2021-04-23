## Data, functions and packages
source(file="0.packages.R")
source(file="functions/BPfunc_month.R")
source(file="functions/get_plots.R")
source(file="functions/heatmaps.R")
load(file="data_proc/data_prec_bimonth.Rdata")

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
  group_by(year,bimonth,station) %>% 
  left_join(estaciones, by=c("station")) %>% 
  group_by(station, month) %>% 
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
         SDII.c = SDII - SDII.m)
         

## Variable Description - Boxplots and Heatmaps

units <- c("NA")

for(mm in 1:12){
plot_list <- colnames(dat)[4:13] %>% 
  map( ~ BPfunc_month(.x, units,mm))
save(plot_list, file=paste0("maps/plot_list_bimonth",mm,"_prec.Rdata"))
}

for(mm in 1:12){
heatmap_list <- colnames(dat)[4:13] %>% 
  map( ~ heatmap_function(.x,mm))
save(heatmap_list, file=paste0("maps/heatmap_list_month",mm,"_prec.Rdata"))
}

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
  dplyr::select(station, year,bimonth, ends_with(".c")) %>% 
  pivot_longer(cols= ends_with(".c"),
               names_to = "variable", 
               values_to = "value") %>% 
  arrange(year) %>% 
  group_by(station,variable,bimonth) %>% 
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
i<-1;m<-1
get_plots(i,m)
summary(trends %>% filter(variable==all[i]))
tab_trends_bimonth_prec <- trends %>% 
  group_by(variable, bimonth) %>% summarize(meanZ=mean(Z)) %>% 
  pivot_wider(names_from=variable, values_from=meanZ)
save(tab_trends_bimonth_prec, file="data_proc/tab_trends_bimonth_prec.Rdata")
