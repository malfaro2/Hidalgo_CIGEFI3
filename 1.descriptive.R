## Data and packages
source(file="0.packages.R")
#source(file="0.functions.R")
load(file="data.Rdata")

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

BPfunc <- function(.x_var, units){
  x_var <- sym(.x_var)
  a <- dat %>%
  ggplot(aes(x=as.factor(year), y=!! x_var)) +
  geom_boxplot() +
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)) +
  xlab("Year") +
  ylab(x_var)  +
  labs(caption=paste("units: ",units))
  return(a)
}

units <- list("days","days","mm","days","days",
              "mm","mm","mm","mm","mm/day")

plot_list <- colnames(dat)[3:12] %>% 
  map( ~ BPfunc(.x, units))

plot_list[[4]]

## Trends - Only calculates trends

cLM <- function(var,year){lm(var ~ year-1)$coef}
cMK <- function(var){as.numeric(MannKendall(ts(var))$tau)}
cpMK <- function(var){as.numeric(MannKendall(ts(var))$sl)}
cpLM <- function(var,year){summary(lm(var ~ year-1))$coefficients[,4]}

head(dat) # transform into station, year, CDD.c ... SDII.c
trends <- dat %>% 
  dplyr::select(station, year, ends_with(".c")) %>% 
  pivot_longer(cols= ends_with(".c"),
               names_to = "variable", 
               values_to = "value") %>% 
  group_by(station, variable) %>% 
  summarize(coefMK = cMK(value),
            coefLM = cLM(value,year),
            pMK = cpMK(value),
            pLM = cpLM(value, year))

## Describe trends:

get.plots<-function(i){
a<-trends %>% dplyr::filter(variable==all[i]) %>% 
  full_join(estaciones, .id = "station") %>% 
  ggplot(aes(-lon, lat)) +
  geom_sf(data = map1, inherit.aes = FALSE) +
  coord_sf(ylim = c(0,25), xlim = c(-70, -110)) +
  geom_point(aes(fill = pLM>0.05, size = coefLM), 
             shape = 21) +
  scale_fill_manual(values=c("red","blue"),
                        limits=c("FALSE","TRUE")) +
  theme_ipsum() + theme(
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 12)) +
  scale_size_area(max_size = 6, guide = "none") +
  labs(title = paste("Central America: coefLM",all[i]), 
  x = "Lon", y = "Lat") 

b<-trends %>% dplyr::filter(variable==all[i]) %>%  
  full_join(estaciones, .id = "station") %>% 
  ggplot(aes(-lon, lat)) +
  geom_sf(data = map1, inherit.aes = FALSE) +
  coord_sf(ylim = c(0,25), xlim = c(-70, -110)) +
  geom_point(aes(fill = pMK>0.05, size = coefMK), 
             shape = 21) +
  scale_fill_manual(values=c("red","blue"),
                      limits=c("FALSE","TRUE")) +
  theme_ipsum() + theme(
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 12)) +
  scale_size_area(max_size = 6, guide = "none") +
  labs(title = paste("Central America: coefMK",all[i]), 
       x = "Lon", y = "Lat") 

return(a+b)
}

all <- unique(trends$variable);all

get.plots(2)

## Now, apply these permutation methods:

# https://climatedataguide.ucar.edu/climate-data-tools-and-analysis/trend-analysis
# https://link.springer.com/article/10.1007/s10651-020-00446-4

set.seed(1818)
per <- lapply(1:1000,function(i)sample(1:32,32,replace=FALSE))

# i <- 1:320 * get location with max 
# j <- 1:10 #variable
# k <- 1:1000
# 0.025 and 0.975 quantiles of k repetitions

## FALTA LA CORRECCIón TEMPORAL 
## series 𝑌𝑡=𝑥𝑡−𝑟̂ 𝑥𝑡−1.
test.se## falta convertir el estadístico S en uno normal
## https://link.springer.com/article/10.1007/s10651-020-00446-4#Sec13

t <- matrix(0,1000,10)
S <- stati <- c()
quant <- matrix(0,2,10)

# for(i in 1:10){
#   for(j in 1:1000){
#     for(k in 1:174){
#       var <- datos[datos$station==k&datos$
#                      variable==all[i],]
#       var <- var$value[per[[j]]]
#       S[k]   <- as.numeric(MannKendall(ts(var))$S)
#     }
#   stati[j] <- max(S)
#   }
#   quant[,i] <- quantile(stati,probs = c(0.025,0.975))
# }
# save(quant, file="quant.Rdata")

load(file="quant.Rdata")

cMK2 <- function(var){as.numeric(MannKendall(ts(var))$S)}

MK_var <- dat %>% 
  dplyr::select(station, year, ends_with(".c")) %>% 
  pivot_longer(cols= ends_with(".c"),
               names_to = "variable", 
               values_to = "value") %>% 
  group_by(station, variable) %>% 
  summarize(coefMK = cMK2(value)) %>% 
  group_by(variable) %>% 
  summarize(maxcMK = max(coefMK)) 

tibble(MK_var, Li=quant[1,], Ls=quant[2,]) %>% 
  mutate(sign = Ls-maxcMK > 0)
  <    
