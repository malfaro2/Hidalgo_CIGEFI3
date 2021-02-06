### PRECIPITATION



## Now, apply these permutation methods:

# https://climatedataguide.ucar.edu/climate-data-tools-and-analysis/trend-analysis
# https://link.springer.com/article/10.1007/s10651-020-00446-4

set.seed(1818)
per <- lapply(1:1000,function(i)sample(1:N,N,replace=FALSE))

# i <- 1:N * get location with max 
# j <- 1:10 #variable
# k <- 1:1000
# 0.025 and 0.975 quantiles of k repetitions

## CORRECCIón TEMPORAL 
## series 𝑌𝑡=𝑥𝑡−𝑟̂ 𝑥𝑡−1.
## https://link.springer.com/article/10.1007/s10651-020-00446-4#Sec13

stati <- matrix(0,1000,9)

dat8s <- dat %>% 
  dplyr::select(station, year, ends_with(".c")) %>% 
  pivot_longer(cols= ends_with(".c"),
               names_to = "variable", 
               values_to = "value")

hist(trends$r)
#trends$r[which(is.na(trends$r)==TRUE)]=1

get.S <- function(k,v1,pp){
  r   <- (trends %>% filter(station==k,variable==v1))$r
  var <- (datos %>% filter(station==k,variable==v1))$value[pp]
  Y   <- var - r*c(0,var[-1])
  return(cMKs(Y))
}

## caution! it takes a while
stati<-lapply(1:10,func8on(z){v1 <- all[z];print(z);
unlist(lapply(1:100,function(y){pp <- per[[y]];print(y);
max(unlist(lapply(1:38, function(x)get.S(x,v1,pp))))}))})

quant <- sapply(stati,function(x){quantile(x,probs = c(0.025,0.975))})
save(quant, file="quant_month_prec.Rdata")

load(file="quant_month_prec.Rdata")

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

tab_final2<-tibble(MK_var, Li=quant[1,], Ls=quant[2,]) %>% 
  mutate(sign = Ls-maxcMK < 0)

library(knitr)
kable(tab_final2)


#### TEMPERATURE



## Now, apply these permutation methods:

# https://climatedataguide.ucar.edu/climate-data-tools-and-analysis/trend-analysis
# https://link.springer.com/article/10.1007/s10651-020-00446-4

set.seed(1818)
per <- lapply(1:1000,function(i)sample(1:35,35,replace=FALSE))

# i <- 1:38 * get location with max 
# j <- 1:10 #variable
# k <- 1:1000
# 0.025 and 0.975 quantiles of k repetitions

## CORRECCIón TEMPORAL 
## series 𝑌𝑡=𝑥𝑡−𝑟̂ 𝑥𝑡−1.
## https://link.springer.com/article/10.1007/s10651-020-00446-4#Sec13

stati <- matrix(0,1000,9)

datos <- dat %>% 
  dplyr::select(station, year, ends_with(".c")) %>% 
  pivot_longer(cols= ends_with(".c"),
               names_to = "variable", 
               values_to = "value")

hist(trends$r)
#trends$r[which(is.na(trends$r)==TRUE)]=1

get.S <- function(k,v1,pp){
  r   <- (trends %>% filter(station==k,variable==v1))$r
  var <- (datos %>% filter(station==k,variable==v1))$value[pp]
  Y   <- var - r*c(0,var[-1])
  return(cMKs(Y))
}

## caution! it takes a while
stati<-lapply(1:9,funct10on(z){v1 <- all[z];print(z);
unlist(lapply(1:100,function(y){pp <- per[[y]];print(y);
max(unlist(lapply(1:38, function(x)get.S(x,v1,pp))))}))})

quant <- sapply(stati,function(x){quantile(x,probs = c(0.025,0.975))})
save(quant, file="quant_month_temp.Rdata")

load(file="quant2_month_temp.Rdata")

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

tab_final2<-tibble(MK_var, Li=quant[1,], Ls=quant[2,]) %>% 
  mutate(sign = Ls-maxcMK < 0)

library(knitr)
kable(tab_final2)