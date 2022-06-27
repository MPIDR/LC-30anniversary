## deriving LC forecast rates

## cleaning the workspace
rm(list=ls(all=TRUE))

## loading functions
source("src/00-functions.R")

## reading data
pars_age <- read.table("dat/age-pars.txt",header = T)
pars_time <- read.table("dat/time-pars.txt",header = T)

## extract ages and years
x <- pars_age$age
tF <- pars_time$year
m <- length(x)
nF <- length(tF)

## LC parameters
ax <- pars_age$ax
bx <- pars_age$bx
kt <- pars_time$kt
sd <- pars_time$sd

## plotting pars
par(mfrow=c(1,3))
plot(x,ax,t="l")
plot(x,bx,t="l")
plot(tF,kt,t="l")
par(mfrow=c(1,1))

## deriving LC forecasts
LMX.lc1 <- LCeta(ax,bx,kt)
matplot(x,LMX.lc1,t="l",lty=1,col=rainbow(nF))

## Coale and Guo adjustment
LMX.lc <- log(apply(exp(LMX.lc1),2,CoaleGuoAdj,x=x))
rownames(LMX.lc) <- x
colnames(LMX.lc) <- tF
matplot(x,LMX.lc,t="l",lty=1,col=rainbow(nF))

## save LC forecasts
save(LMX.lc,ax,bx,kt,sd,x,tF,file = "out/10-LCforecast.Rdata")

