######################
### PRECIPITATION ####
######################

rm(list=ls())
source(file="1.descriptive_year_prec.R")
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
  dplyr::select(station, year, ends_with(".c")) %>% 
  pivot_longer(cols= ends_with(".c"),
               names_to = "variable", 
               values_to = "value")

hist(trends$r)
#trends$r[which(is.na(trends$r)==TRUE)]=1

# k = 1 #38 total stations
# v1 = "CDD.c" # 10 indexes in total
# pp = it's the permuted set, 101 is the real one.
get.S <- function(k,v1,pp){
  r   <- (trends %>% filter(station==k,variable==v1))$r
  var <- (datos %>% filter(station==k,variable==v1) %>% 
            arrange(year))$value[pp]
  Y   <- var - r*c(0,var[-1])
  return(cMKs(Y))
}

## value 101 is the real value
## caution! it takes a while
#stati is a list of 10 variables, each w/ matrix(0,38,101)
# stati<-lapply(1:10,function(z){v1 <- all[z];print(v1);
# (sapply(1:101,function(y){pp <- per[[y]];print(y);
# ((sapply(1:38, function(x)get.S(x,v1,pp))))}))})
# save(stati, file="quant_year_prec.Rdata")
load(file="quant_year_prec.Rdata")

## a list of 10 variables, each with 38 Kendall S Scores,
## and the 2.5% and 97.5% percentiles of S for each location

local <-lapply(1:10,function(x){cbind(stati[[x]][,101],
      apply(stati[[x]],1,function(y)quantile(y,probs=0.025)),
      apply(stati[[x]],1,function(y)quantile(y,probs=0.975)))})

## a list of 10 variables, each with one max Kendall Scores,
## and the 2.5% and 97.5% percentiles of max(S) for all locations

calc_max <- unlist(lapply(1:10,function(x){max(stati[[x]][,101])}))
tab_global <- sapply(1:10,function(x){apply(stati[[x]],2,max)})
global <- tibble(Var=all,Max.S = calc_max,
          P2.5= apply(tab_global,2,function(y)quantile(y,probs=0.025)),
          P97.5= apply(tab_global,2,function(y)quantile(y,probs=0.975)))

tab_final2<-global %>% 
  mutate(signif = P97.5-Max.S < 0)

kable(tab_final2)


######################
###  TEMPERATURE  ####
######################

rm(list=ls())
source(file="1.descriptive_year_temp.R")

TL <- max(dat$year)
set.seed(1818)
per <- lapply(1:100,function(i)sample(1:TL,TL,replace=FALSE))
per[[101]] <- 1:TL

## Temporal correction
## series 𝑌𝑡=𝑥𝑡−𝑟̂ 𝑥𝑡−1.
## https://link.springer.com/article/10.1007/s10651-020-00446-4#Sec13

datos <- dat %>% 
  dplyr::select(station, year, ends_with(".c")) %>% 
  pivot_longer(cols= ends_with(".c"),
               names_to = "variable", 
               values_to = "value")

hist(trends$r)
#trends$r[which(is.na(trends$r)==TRUE)]=1

# k = 1 #38 total stations
# v1 = "DTR.c" # 9 indexes in total
# pp = it's the permuted set, 101 is the real one.
get.S <- function(k,v1,pp){
  r   <- (trends %>% filter(station==k,variable==v1))$r
  var <- (datos %>% filter(station==k,variable==v1) %>% 
            arrange(year))$value[pp]
  Y   <- var - r*c(0,var[-1])
  return(cMKs(Y))
}

## value 101 is the real value
## caution! it takes a while
#stati is a list of 9 variables, each w/ matrix(0,38,101)
# stati<-lapply(1:9,function(z){v1 <- all[z];print(v1);
# (sapply(1:101,function(y){pp <- per[[y]];print(y);
# ((sapply(1:38, function(x)get.S(x,v1,pp))))}))})
# save(stati, file="quant_year_temp.Rdata")
load(file="quant_year_temp.Rdata")

## a list of 9 variables, each with 38 Kendall S Scores,
## and the 2.5% and 97.5% percentiles of S for each location

local <-lapply(1:9,function(x){cbind(stati[[x]][,101],
       apply(stati[[x]],1,function(y)quantile(y,probs=0.025)),
       apply(stati[[x]],1,function(y)quantile(y,probs=0.975)))})

## a list of 9 variables, each with one max Kendall Scores,
## and the 2.5% and 97.5% percentiles of max(S) for all locations

calc_max <- unlist(lapply(1:9,function(x){max(stati[[x]][,101])}))
tab_global <- sapply(1:9,function(x){apply(stati[[x]],2,max)})
global <- tibble(Var=all,Max.S = calc_max,
        P2.5= apply(tab_global,2,function(y)quantile(y,probs=0.025)),
        P97.5= apply(tab_global,2,function(y)quantile(y,probs=0.975)))

tab_final2<-global %>% 
  mutate(signif = P97.5-Max.S < 0)

kable(tab_final2)
