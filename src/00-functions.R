## General functions used throughout

## function for constructing a classic (& rather general) lifetable
## from mortality rates
lifetable.mx <- function(x, mx, sex="M", ax=NULL){
  m <- length(x)
  n <- c(diff(x), NA)
  if(is.null(ax)){
    ax <- rep(0,m)
    if(x[1]!=0 | x[2]!=1){
      ax <- n/2
      ax[m] <- 1 / mx[m]
    }else{    
      if(sex=="F"){
        if(mx[1]>=0.107){
          ax[1] <- 0.350
        }else{
          ax[1] <- 0.053 + 2.800*mx[1]
        }
      }
      if(sex=="M"){
        if(mx[1]>=0.107){
          ax[1] <- 0.330
        }else{
          ax[1] <- 0.045 + 2.684*mx[1]
        }
      }
      ax[-1] <- n[-1]/2
      ax[m] <- 1 / mx[m]
    }
  }
  qx  <- n*mx / (1 + (n - ax) * mx)
  qx[m] <- 1
  px  <- 1-qx
  lx  <- cumprod(c(1,px))*100000
  dx  <- -diff(lx)
  Lx  <- n*lx[-1] + ax*dx
  lx <- lx[-(m+1)]
  Lx[m] <- lx[m]/mx[m]
  Lx[is.na(Lx)] <- 0 ## in case of NA values
  Lx[is.infinite(Lx)] <- 0 ## in case of Inf values
  Tx  <- rev(cumsum(rev(Lx)))
  ex  <- Tx/lx
  return.df <- data.frame(x, n, mx, ax, qx, px, lx, dx, Lx, Tx, ex)
  return(return.df)
}

## function to derive life expectancy (e0) 
## from mx (based on previous function)
e0.mx <- function(x, mx, sex="M", ax=NULL){
  lt <- lifetable.mx(x,mx, sex,ax)
  return.ex <- lt$ex[1]
  return(return.ex)
}

## function to compute LC log-rates from parameters
LCeta <- function(ax,bx,kt){
  n <- length(kt)
  One <- matrix(1,n,1)
  ETA <- ax%*%t(One) + bx%*%t(kt)
  return(ETA)
}

## adjusting rates at ages 85+
CoaleGuoAdj <- function(x,mx){
  ## dimensions
  m <- length(x)
  mAdj <- m-length(x[x<=80])
  ## extract rate at age 75 and 80
  mx75 <- mx[x==75]
  mx80 <- mx[x==80]
  ## compute mx at age 105
  mx105 <- mx75+0.66
  ## compute k80
  k80 <- log(mx80/mx75)
  ## compute R
  R <- (6*k80-log(mx105/mx75))/15
  ## compute different ks
  ks <- k80 - seq(1,mAdj)*R
  ## adjust rates 85+
  mxAdj <- mx80*exp(cumsum(ks))
  mxNew <- c(mx[1:(m-mAdj)],mxAdj)
  return(mxNew)
}





