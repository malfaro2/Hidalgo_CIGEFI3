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

trendfunc <- function(.x_var){
  x_var <- sym(.x_var)
  a <- dat %>% arrange(station) %>% 
  nest(data = c(year, CDD, CWD, PRCPTOT, R10mm, R20mm, 
                      R95p, R99p,RX1day, RX5day, SDII,
                CDD.m, CWD.m, PRCPTOT.m, R10mm.m, R20mm.m, 
                R95p.m, R99p.m, RX1day.m, RX5day.m, SDII.m,
                CDD.c, CWD.c, PRCPTOT.c, R10mm.c, R20mm.c, 
                R95p.c, R99p.c, RX1day.c, RX5day.c, SDII.c)) %>% 
  mutate(coef = purrr::map(data, ~ lm(.x_var ~ year-1,data = .)$coef),
         p.mk = purrr::map(data, ~ as.numeric(MannKendall
                                    (ts(.$x_var))$sl))) %>% 
         tidyr::unnest(coef) %>% tidyr::unnest(p.mk) %>% 
         dplyr::select(station,coef,p.mk) %>% 
         left_join(estaciones, by=c("station"))
  return(a)
}



