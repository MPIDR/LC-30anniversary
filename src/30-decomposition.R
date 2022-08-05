## decomposition of e0 difference by so-called Horiuchi's approach
## difference is decomposed using 
## actual rates in 2019 and forecast by LC as provided in the original 1992 paper

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
## summing age-specific decomposed contributions
(check2 <- sum(A))

## plotting contributions
age_low <- seq(5, 105,5)
age_up <- age_low-1
age_lab <- c("0", paste(c(1, age_low), c(age_up,109), sep="-"))
age_group <- factor(x,labels=age_lab)


my.df <- tibble(x=x,agegroup=age_group,A=A,sign=sign(A))

my.df %>% 
  mutate(sign=factor(sign,labels = c("negative","positive"))) %>% 
  ggplot(aes(y=agegroup,x=abs(A),fill=sign)) +
  geom_col() +
  labs(x="contribution",y="age group",fill="contr.")+
  theme_bw(base_size = 14)

ggsave("out/30-figure_decomposition.pdf")

