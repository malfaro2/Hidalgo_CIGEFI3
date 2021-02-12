######################
### PRECIPITATION ####
######################

rm(list=ls())
source(file="1.descriptive_month_prec.R")

## Trend test using permutation methods:
# https://climatedataguide.ucar.edu/climate-data-tools-and-analysis/trend-analysis
# https://link.springer.com/article/10.1007/s10651-020-00446-4

TL <- max(dat$year)
set.seed(1818)
per <- lapply(1:100,function(i)sample(1:TL,TL,replace=FALSE))
per[[101]] <- 1:TL

## Temporal correction
## series 𝑌𝑡=𝑥𝑡−𝑟̂ 𝑥𝑡−1.
## https://link.springer.com/article/10.1007/s10651-020-00446-4#Sec13

datos <- dat %>% 
  dplyr::select(station, year, month,ends_with(".c")) %>% 
  pivot_longer(cols= ends_with(".c"),
               names_to = "variable", 
               values_to = "value")

hist(trends$r)

# k = 1 #174 total stations
# v1 = "CDD.c" # 8 indexes in total
# pp = it's the permuted set, 101 is the real one.
get.S <- function(k,v1,pp,mm){
  r   <- (trends %>% filter(station==k,variable==v1,month==mm))$r
  var <- (datos %>% filter(station==k,variable==v1, month==mm) %>% 
    arrange(year))$value[pp]
  Y   <- var - r*c(0,var[-1])
  return(cMKs(Y))
}

## value 101 is the real value
## caution! it takes a while
#stati is a list of 8 variables, each a list of 12 months, each w/ matrix(0,174,101)
stati<-lapply(1:8,function(z){v1 <- all[z];print(v1);
lapply(1:12,function(w){#months
(sapply(1:101,function(y){pp <- per[[y]];print(y);
((sapply(1:174, function(x)get.S(x,v1,pp,w))))}))})})
save(stati, file="data_proc/quant_month_prec.Rdata")
load(file="data_proc/quant_month_prec.Rdata")

## a list of 8 variables, each with 174 Kendall S Scores,
## and the 2.5% and 97.5% percentiles of S for each location

for(i in 1:12){
  tab_local_month_prec <-lapply(1:8,function(x){cbind(stati[[x]][[i]][,101],
      apply(stati[[x]][[i]],1,function(y)quantile(y,probs=0.025)),
      apply(stati[[x]][[i]],1,function(y)quantile(y,probs=0.975)))})
 save(tab_local_month_prec,all, 
      file=print0("data_proc/tab_local_month",i,"_prec.Rdata")) 
}

## a list of 8 variables, each with one max Kendall Scores,
## and the 2.5% and 97.5% percentiles of max(S) for all locations

for(i in 1:12){
calc_max <- unlist(lapply(1:8,function(x){max(stati[[x]][[i]][,101])}))
tab_global <- sapply(1:8,function(x){apply(stati[[x]][[i]],2,max)})
global <- tibble(Var=all,Max.S = calc_max,
     P2.5= apply(tab_global,2,function(y)quantile(y,probs=0.025)),
     P97.5= apply(tab_global,2,function(y)quantile(y,probs=0.975)))
tab_global_month_prec<-global %>% 
  mutate(signif = P97.5-Max.S < 0)
save(tab_global_month_prec, 
     file=print0("data_proc/tab_global_month",i,"_prec.Rdata"))
}
######################
####  TEMPERATURE ####
######################

rm(list=ls())
source(file="1.descriptive_month_temp.R")

TL <- max(dat$year)*max(dat$month)
set.seed(1818)
per <- lapply(1:100,function(i)sample(1:TL,TL,replace=FALSE))
per[[101]] <- 1:TL

datos <- dat %>% 
  dplyr::select(station, year, month, ends_with(".c")) %>% 
  pivot_longer(cols= ends_with(".c"),
               names_to = "variable", 
               values_to = "value")

hist(trends$r)

# k = 1 #N=38 total
# v1 = "DTR.c" # 9 in total
# pp = it's the permuted set, 101 is the real one.
get.S <- function(k,v1,pp,mm){
  r   <- (trends %>% filter(station==k,variable==v1,month==mm))$r
  var <- (datos %>% filter(station==k,variable==v1, month==mm) %>% 
            arrange(year))$value[pp]
  Y   <- var - r*c(0,var[-1])
  return(cMKs(Y))
}

## value 101 is the real value
## caution! it takes a while
#stati is a list of 9 variables, each a list of 12 months, each w/ matrix(0,174,101)
stati<-lapply(1:9,function(z){v1 <- all[z];print(v1);
lapply(1:12,function(w){#months
(sapply(1:101,function(y){pp <- per[[y]];print(y);
((sapply(1:N, function(x)get.S(x,v1,pp,w))))}))})})
save(stati, file="data_proc/quant_month_temp.Rdata")
load(file="data_proc/quant_month_temp.Rdata")

## a list of 9 variables, each with 38 Kendall Scores,
## and the 2.5% and 97.5% percentiles of S for each location

for(i in 1:12){
tab_local_month_temp <-lapply(1:9,function(x){cbind(stati[[x]][[i]][,101],
        apply(stati[[x]][[i]],1,function(y)quantile(y,probs=0.025)),
        apply(stati[[x]][[i]],1,function(y)quantile(y,probs=0.975)))})
save(tab_local_month_temp,all, 
     file=print0("data_proc/tab_local_month",i,"_temp.Rdata"))
}

## a list of 9 variables, each with one max Kendall Scores,
## and the 2.5% and 97.5% percentiles of max(S) for all locations

for(i in 1:12){
calc_max <- unlist(lapply(1:9,function(x){max(stati[[x]][[i]][,101])}))
tab_global <- sapply(1:9,function(x){apply(stati[[x]][[i]],2,max)})
global <- tibble(Var=all,Max.S = calc_max,
 P2.5= apply(tab_global,2,function(y)quantile(y,probs=0.025)),
 P97.5= apply(tab_global,2,function(y)quantile(y,probs=0.975)))
tab_global_month_temp<-global %>% 
  mutate(signif = P97.5-Max.S < 0)
save(tab_global_month_temp, 
     file=print0("data_proc/tab_global_month",i,"_temp.Rdata"))
}


