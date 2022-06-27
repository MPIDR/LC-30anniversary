## checking statements in the manuscript

## cleaning the workspace
rm(list=ls(all=TRUE))

## loading data
load("out/20-evaluation.Rdata")

## checking Table 4 in LC paper - values identical until age 85-89,
## small errors later (probably due to Coale and Guo adjustment)
my.year <- 2010
round(exp(LMX.lc[,which(tF==my.year)])*100000)

## e0 diff in first fore year
tF[1]
e0lc[1]-e0obs[which(t==tF[1])]

## e0 diff in last fore year
tF[nF]
e0lc[nF]-e0obs[which(t==tF[nF])]

## coverage of 95 PI
df <- tibble(mx2019.obs,mx2019.lc.up,mx2019.lc.low)
df <- df %>% 
  mutate(coverage=ifelse((mx2019.obs<=mx2019.lc.up)&(mx2019.obs>=mx2019.lc.low),1,0))
sum(df$coverage)
sum(df$coverage)/nrow(df)
