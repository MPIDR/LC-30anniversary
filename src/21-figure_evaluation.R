## plotting the evaluation figure

## cleaning the workspace
rm(list=ls(all=TRUE))

## loading data
load("out/20-evaluation.Rdata")

## some graphical parameters 
ylimE0 <- range(e0obs,e0lc,e0lc.up,e0lc.low)
ylimMX <- range(mx2019.obs,mx2019.lc,mx2019.lc.up,mx2019.lc.low)
col.obs <- "grey30"
col.lc <- 4
col.lcT <- adjustcolor(col.lc,alpha.f = 0.3)
cex.main <- 1.65
cex.axis.title <- 1.4
cex.axis.mx <- 0.75
SAVE.FIG <- T

if (SAVE.FIG){
  pdf(file="out/21-figure_evaluation.pdf",width=10,height = 6.5)
} 

## plot
par(mfrow = c(1,2),oma = c(1,1,0.5,0),
    mar = c(2.1,2,1.5,1))
## bottom, left, top, right

## panel 1: LE
plot(t,e0obs,t="n",ylim=ylimE0,axes=F,xlab="",ylab="")
axis(1,padj = -0.5);axis(2,las=2,hadj = .75);grid();box()
title("Life expectancy at birth",cex.main=cex.main,line = 0.35)
points(t,e0obs,pch=16,col=col.obs)
xx <- c(tF,rev(tF))
yy <- c(e0lc.up,rev(e0lc.low))
polygon(xx,yy,col=col.lcT,border = col.lcT)
lines(tF,e0lc,col=col.lc,lwd=2)
mtext("year", 1, line=1.75,cex=cex.axis.title)
mtext(expression(e[0]), 2, line=1.5,cex=cex.axis.title,las=2)
legend("topleft",legend=c("Observed","Lee-Carter (1992)"),lwd=2,cex=1.25,
       pch=c(16,NA),lty=c(NA,1),
       col=c(col.obs,col.lc),bg="white",inset=0.05)
legend("topleft",legend=c("","", ""),pch=c(NA,15),col=c(NA,col.lcT),
       bty="n",lwd=NA,cex=1.25,lty=NA,inset=0.05)


## panel 2: mx
plot(x,mx2019.obs,t="n",ylim=ylimMX,axes=F,xlab="",ylab="",log="y")
axis(2,las=2,hadj = .75,at=c(1e-4,1e-3,1e-2,1e-1),
     labels = c("0.0001","0.001","0.01","0.1"),cex.axis=cex.axis.mx)
axis(1,padj = -0.5,at=seq(0,100,20),
     labels=c("0","20-24","40-44","60-64","80-84","100-104"));grid();box()
title("Death rates, year 2019",cex.main=cex.main,line = 0.35)
points(x,mx2019.obs,pch=16,col=col.obs)
xx <- c(x,rev(x))
yy <- c(mx2019.lc.low,rev(mx2019.lc.up))
polygon(xx,yy,col=col.lcT,border = col.lcT)
lines(x,mx2019.lc,col=col.lc,lwd=2)
mtext("age group", 1, line=1.75,cex=cex.axis.title)
mtext(expression(m[x]), 2, line=1,cex=cex.axis.title,las=2)
par(mfrow=c(1,1))

if (SAVE.FIG) dev.off()

