## Data and packages
source(file="0.packages.R")
source(file="functions/BPfunc_year.R")
source(file="functions/get_plots.R")
load(file="data_prec_year.Rdata")

# Draw plots and calculate descriptive stats

estaciones <- as_tibble(locations)
estaciones$station <- 1:174
names(estaciones) <- c("lat","lon","station")
estaciones$station <- factor(estaciones$station, levels=c(1:174))
map1 <- rnaturalearth::ne_states(
  country = c("guatemala", "honduras", 
              "el salvador", "panama", 
              "nicaragua", "costa rica", 
              "belize", "mexico", 
              "colombia"), returnclass = "sf")


names(datos)
datos$station <- factor(datos$station, levels=c(1:174))

dat <- datos %>% 
  group_by(year,station) %>% 
  left_join(estaciones, by=c("station")) %>% 
  group_by(station) %>% 
  mutate(CDD.m = mean(CDD,na.rm=TRUE),
         CWD.m = mean(CWD,na.rm=TRUE),
         PRCPTOT.m = mean(PRCPTOT,na.rm=TRUE),
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
         PRCPTOT.c = PRCPTOT - PRCPTOT.m,
         R10mm.c = R10mm - R10mm.m,
         R20mm.c = R20mm - R20mm.m,
         R95p.c =  R95p -  R95p.m,
         R99p.c =  R99p -  R99p.m,
         RX1day.c = RX1day - RX1day.m,
         RX5day.c = RX5day - RX5day.m,
         SDII.c = SDII - SDII.m)

## Variable Description - Boxplots

units <- list("days","days","mm","days","days",
              "mm","mm","mm","mm","mm/day")
plot_list <- colnames(dat)[3:12] %>% 
  map( ~ BPfunc_year(.x, units))
plot_list[[1]]

jpeg("maps/desc_prec_year1.jpg", width = 1200, height = 750)
(plot_list[[1]] | plot_list[[2]]) /
  (plot_list[[3]] | plot_list[[4]]) 
dev.off()
jpeg("maps/desc_prec_year2.jpg", width = 1200, height = 750)
(plot_list[[5]] | plot_list[[6]]) /
  (plot_list[[7]] | plot_list[[8]])
dev.off()
jpeg("maps/desc_prec_year3.jpg", width = 1200, height = 325)
(plot_list[[9]] | plot_list[[10]]) 
dev.off()

## Trends - Only calculates trends and correlation

cMKt <- function(var){as.numeric(MannKendall(ts(var))$tau)}
cMKs <- function(var){as.numeric(MannKendall(ts(var))$S)}
cMKv <- function(var){as.numeric(MannKendall(ts(var))$varS)}
pMK  <- function(var){as.numeric(MannKendall(ts(var))$sl)}

head(dat) # transform into station, year, CDD.c ... SDII.c
trends <- dat %>% 
  dplyr::select(station, year, ends_with(".c")) %>% 
  pivot_longer(cols= ends_with(".c"),
               names_to = "variable", 
               values_to = "value") %>% 
  arrange(year) %>% 
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
i<-4
get_plots(i)
summary(trends %>% filter(variable==all[i]))
trends %>% group_by(variable) %>% summarize(meanZ=mean(Z))
