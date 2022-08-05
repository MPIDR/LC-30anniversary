## performing evaluation 
## between actual rates and life expectancy (from the Human Mortality Database) 
## and values produced using parameters and approaches presented
## in the original 1992 paper

## (See Section 3 in the manuscript)

## cleaning the workspace
rm(list=ls(all=TRUE))

## loading usieful packages
library(HMDHFDplus)
library(tidyverse)

## loading functions and LC data
source("src/00-functions.R")
load("out/10-LCforecast.Rdata")

## dimensions
m <- length(x)
nF <- length(tF)

## adding five additional years for plot
t1 <- (tF[1]-5):(tF[1]-1)  ## 5 years before forecast for later plot
t <- c(t1,tF)
nF <- length(tF)
n <- length(t)

## downloading US data from HMD
## (insert your HMD username and password)
username <- "username"
password <- "password"
Mx.Usa <- readHMDweb(CNTRY="USA",item="Mx_5x1",
                     username=username, password=password, fixup=T)

## subset years of interest and remove year 110 (not used by LC)
my.df <- Mx.Usa %>% 
  select(Year,Age,Total) %>% 
  filter(Year %in% t) %>% 
  filter(Age!=110)

## observed (log-)rates
MX.obs <- matrix(my.df$Total,m,n)
LMX.obs <- log(MX.obs)

## computing e0 for each year
e0obs <- apply(MX.obs, 2, e0.mx, x=x)
e0lc <- apply(exp(LMX.lc), 2, e0.mx, x=x)

## plot observed and LC e0
plot(t,e0obs,t="p",ylim=range(e0obs,e0lc))
lines(tF,e0lc,col=2)

## computing PIs of kt
c.val <- qnorm(0.975)
kt.up <- kt + c.val*sd
kt.low <- kt - c.val*sd
plot(tF,kt,t="l",ylim = range(kt.up,kt.low))
lines(tF,kt.up,lty=2,col=2)
lines(tF,kt.low,lty=2,col=2)

## compute lower and upper log-rates 
LMX.lc.up1 <- LCeta(ax, bx, kt.up)
LMX.lc.low1 <- LCeta(ax, bx, kt.low)

## Coale and Guo adjustment (see 00-functions and 10-LCforecast)
LMX.lc.up <- log( apply(exp(LMX.lc.up1), 2, CoaleGuoAdj, x=x) )
LMX.lc.low <- log( apply(exp(LMX.lc.low1), 2, CoaleGuoAdj, x=x) )

## compute lower and upper e0
e0lc.up <- apply(exp(LMX.lc.low),2,e0.mx,x=x)
e0lc.low <- apply(exp(LMX.lc.up),2,e0.mx,x=x)

## rates in 2019
lmx2019.obs <- LMX.obs[,which(t==tF[nF])]
lmx2019.lc <- LMX.lc[,nF]

## adjustment as described by LC (see Appendix B, p. 669-670)
lmx2019.lc.up <- log(exp(lmx2019.lc)*exp(c.val*bx*sd[nF]))
lmx2019.lc.low <- log(exp(lmx2019.lc)*exp(-c.val*bx*sd[nF]))

## transform to rates
mx2019.obs <- exp(lmx2019.obs)
mx2019.lc <-  exp(lmx2019.lc)
mx2019.lc.up <- exp(lmx2019.lc.up)
mx2019.lc.low <- exp(lmx2019.lc.low)

## save results
save.image(file = "out/20-evaluation.Rdata")

