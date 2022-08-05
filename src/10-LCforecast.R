## deriving LC forecast rates
## using parameters and approaches presented in the original 1992 paper:
## Lee, R. D. and Carter, L. R. (1992)
## Modeling and Forecasting U.S. Mortality
## Journal of the American Statistical Association, 87(419):659–671

## cleaning the workspace
rm(list=ls(all=TRUE))

## loading functions
source("src/00-functions.R")

## reading data
## a_x and b_x. Table 1 (p. 662)
pars_age <- read.table("dat/age-pars.txt",header = T)
## k_t and associated standard deviation (up to 2019). Table 2 (p. 664)
pars_time <- read.table("dat/time-pars.txt",header = T)

## extract ages and years (and dimensions)
x <- pars_age$age
tF <- pars_time$year
m <- length(x)
nF <- length(tF)

## extract LC parameters
ax <- pars_age$ax
bx <- pars_age$bx
kt <- pars_time$kt
sd <- pars_time$sd

## plotting LC parameters
par(mfrow=c(1,3))
plot(x,ax,t="l")
plot(x,bx,t="l")
plot(tF,kt,t="l")
par(mfrow=c(1,1))

## apply the LC model with Lee & Carter (1992) parameters
## to obtain log-rates over age (x) and years (tF)
LMX.lc1 <- LCeta(ax,bx,kt)
## plotting log-rates
matplot(x,LMX.lc1,t="l",lty=1,col=rainbow(nF))

## Coale and Guo adjustment 
## "to extend our death rates up to age group 105-109" (p. 662)
LMX.lc <- log(apply(exp(LMX.lc1), 2, CoaleGuoAdj, x=x))
rownames(LMX.lc) <- x
colnames(LMX.lc) <- tF
## plotting (adjusted) log-rates
matplot(x,LMX.lc,t="l",lty=1,col=rainbow(nF))

## save LC forecasts and parameters
save(LMX.lc,ax,bx,kt,sd,x,tF, file = "out/10-LCforecast.Rdata")

