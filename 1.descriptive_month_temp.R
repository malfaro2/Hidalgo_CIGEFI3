## Data and packages
source(file="0.packages.R")
#source(file="0.functions.R")
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
         time = paste0(year,".",month))

## Variable Description - Boxplots

BPfunc <- function(.x_var, units){
  x_var <- sym(.x_var)
  a <- dat %>%
  ggplot(aes(x=as.factor(time), y=!! x_var)) +
  geom_boxplot() +
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)) +
  xlab("Time") +
  ylab(x_var)  +
  labs(caption=paste("units: ",units))
  return(a)
}

units <- c("NA")
plot_list <- colnames(dat)[4:13] %>% 
  map( ~ BPfunc(.x, units))

plot_list[[3]]

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

get.plots<-function(i){
a<-trends %>% dplyr::filter(variable==all[i]) %>% 
  full_join(estaciones, .id = "station") %>% 
  ggplot(aes(lon, lat)) +
  geom_sf(data = map1, inherit.aes = FALSE) +
  coord_sf(ylim = c(0,25), xlim = c(-110, -70)) +
  geom_point(aes(fill = pMK>0.05, size = (abs(tauMK)+0.01)*100), 
             shape = 21) +
  scale_fill_manual(values=c("red","blue"),
                        limits=c("FALSE","TRUE")) +
  theme_ipsum() + theme(
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 12)) +
  scale_size_area(max_size = 6, guide = "none") +
  labs(title = paste("Central America: tau and p",all[i]), 
  x = "Lon", y = "Lat") 

b<-trends %>% dplyr::filter(variable==all[i]) %>%  
  full_join(estaciones, .id = "station") %>% 
  ggplot(aes(lon, lat)) +
  geom_sf(data = map1, inherit.aes = FALSE) +
  coord_sf(ylim = c(0,25), xlim = c(-110, -70)) +
  geom_point(aes(fill = pZ>0.05, size = abs(Z)+0.01), 
             shape = 21) +
  scale_fill_manual(values=c("red","blue"),
                      limits=c("FALSE","TRUE")) +
  theme_ipsum() + theme(
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 12)) +
  scale_size_area(max_size = 6, guide = "none") +
  labs(title = paste("Central America: Z from KM",all[i]), 
       x = "Lon", y = "Lat") 

return(a+b)
}

all <- unique(trends$variable);all

i<-1
get.plots(i)
summary(trends %>% filter(variable==all[i]))

trends %>% group_by(variable) %>% summarize(meanZ=mean(Z))
