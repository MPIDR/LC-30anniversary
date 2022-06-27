## decomposition of e0 difference

## cleaning the workspace
rm(list=ls(all=TRUE))

## loading data
load("out/20-evaluation.Rdata")

## loading useful packages
library(DemoDecomp)
library(tidyverse)

## testing a decomposition of difference in e0
mx1 <- mx2019.obs
mx2 <- mx2019.lc
e0.mx
A <- horiuchi(func = e0.mx,
              pars1 = mx1,
              pars2 = mx2,
              N = 10,
              x = x)
## original difference
(check1 <- e0.mx(x=x,mx=mx2) - e0.mx(x=x,mx=mx1))
(check2 <- sum(A))

## plotting contributions
age_group <- factor(x,labels=c("0",
               "1-4",
               "5-9",
               "10-14",
               "15-19",
               "20-24",
               "25-29",
               "30-34",
               "35-39",
               "40-44",
               "45-49",
               "50-54",
               "55-59",
               "60-64",
               "65-69",
               "70-74",
               "75-79",
               "80-84",
               "85-89",
               "90-94",
               "95-99",
               "100-104",
               "105-109"))

my.df <- tibble(x=x,agegroup=age_group,A=A,sign=sign(A))

my.df %>% 
  mutate(sign=factor(sign,labels = c("negative","positive"))) %>% 
  ggplot(aes(y=agegroup,x=abs(A),fill=sign)) +
  geom_col() +
  labs(x="contribution",y="age group",fill="contr.")+
  theme_bw(base_size = 14)

