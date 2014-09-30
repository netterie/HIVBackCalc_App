#' Estimates the discrete time probability distribution function for
#' the time between infection and diagnosis
#' @param infPeriod a series of times from last HIV test to diagnosis
#' @param intLength the length of the discrete time intervals
estimateProbDist <- function(infPeriod,intLength=1){
  ti <- sort(infPeriod[!is.na(infPeriod) & infPeriod>0])
  #continuous density of time between infection and diagnosis
  pi <- function(i,eta,ti=ti){
    sapply(i,function(ii){
      ints <- ti[ti>=ii]
      sum(1/ints)/length(ti)
    })
  }
  uti <- unique(ti)
  p<-pi(uti,,ti) * diff(c(0,uti))
  cs <- cumsum(p)
  #cdf of density
  qi <- function(u){
    uind <- rev(which(uti<=u))[1]
    if(is.na(uind))
      return(0)
    cs[uind]
  }
  
  #the descrete probability that a diagnosis is made i time units after infection
  pidCalc <- function(i){
    sapply(i,function(ii){
      qi((ii+1)*intLength) - qi(ii*intLength)
    })
  }

  #calc discrete time prob
  m <- max(ti/intLength) + 1
  pidProbs <- pidCalc(0:m)
  pid <- function(i){
    ifelse(i>m,0,pidProbs[i+1])
  }
  pid
}


#' EM update step
#' @param y y
#' @param pid pid
#' @param lambda lambda
#' @param gamma gamma
meanEmUpdate <- function(y,pid,lambda,gamma=0){
  T <- length(y)
  obs <- !is.na(y)
  a <- b <- c <- lamNew <- rep(NA,T)
  for(k in 1:length(lambda)){
    s <- 0:(T-k)
    b[k] <- sum(pid(s))
    no <- !obs[s+k]
    if(any(no))
      a[k] <- sum(pid(s[no])) / b[k]
    else
      a[k] <- 0
    c[k] <- 0
    for(d in s){
      if(obs[k+d]){
        c[k] <- c[k] + y[k+d]*pid(d) / sum(lambda[1:(k+d)]*pid(k+d-(1:(k+d))))
      }
    }
  }
  if(gamma > 0){
    f <- function(ll){
      (1/ll) * (a*b+c) * lambda - b - 2 * gamma * c(0, ll[2:T] - ll[-T]) - 
        2 * gamma * c(ll[1:(T-1)] - ll[-1] ,0)
    }
    j <- function(ll){
      diag <- (-1/ll^2)* (a*b+c)*lambda - 4 * gamma
      diag[1] <- diag[1] + 2 * gamma
      diag[T] <- diag[T] + 2 * gamma
      off <- rep(0,T)
      off[2:(T-1)] <- 2*gamma
      rbind(-off,diag,off)
    }
    lamNew <- multiroot(f=f,start=lambda,positive=TRUE,jacfunc=j,jactype="bandint")$root
  }else{
    lamNew <- lambda * (a + c / b)
  }
  lamNew
}

#' estimates incedence via back calculation
#' @param y diagnosis counts
#' @param pid a function giving the probability of diagnosis given time units from infection
#' @param gamma smoothing penulty
#' @param tol The tolerance for the EM algorithm
#' @param verbose level of verbosity
estimateIncidence <- function(y,pid,gamma=0,tol=10^-5,verbose=FALSE){
  lambda <- rep(mean(y,na.rm=TRUE),length(y))
  ll <- lambda
  dev <- Inf
  while(dev>tol){
    lambda <- meanEmUpdate(y,pid,lambda,gamma)
    dev <- sum((ll-lambda)^2/ll)
    if(verbose){
      cat("lambda: ",paste(round(lambda,1),collapse=" "),"\n",
          "parameter change: ",dev,"\n",sep="")
    }
    ll <- lambda
  }
  mod <- list(lambda=lambda,y=y,pid=pid,gamma=gamma,tol=tol)
  class(mod) <- "backproj"
  mod
}

#' print function
#' @param x object of type backproj
#' @param ... passed to cat
print.backproj <- function(x,...) {
  cat("Back Projection Incidence Model\n","Estimated incidence: ",x$lambda,...)
}

#' plots the model
#' @param x the incedence model
#' @param time a vector of length two giving the start and end times of the observed period
#' @param showDiagCounts should diagnosis counts be plotted
#' @param ... passed to plot
plot.backproj <- function(x,time,showDiagCounts=TRUE, ...){
  obs <- !is.na(x$y)
  if(missing(time)) 
    time <- 1:length(x$y[obs])
  else
    time <- seq(from=time[1],to=time[2],length.out=sum(obs))
  plot(time,x$lambda[obs],ylim=c(0,100),type="l",main="Estimated Incidence",ylab="Count",...)
  if(showDiagCounts)
    points(time,x$y[obs],col="red")
}

#' Estimates the number of undiagnosed in the population
#' @param mod the model
#' @param nExt the number of time units beyond the end of the observed period to use in calculation
estimateUndiagnosed <- function(mod,nExt=500){
  pid <- mod$pid
  lambda <- mod$lambda
  y <- mod$y
  nExt <- 500
  obs <- c(!is.na(y),rep(FALSE,nExt))
  T <- length(y)
  P <- T + nExt
  n <- matrix(NA,nrow=T,ncol=P)
  for(i in 1:T){
    for(j in i:P){
      if(!obs[j]){
        n[i,j] <- lambda[i]*pid(j-i)
      }else{
        n[i,j] <- y[j] * lambda[i]*pid(j-i) / sum( lambda[1:j]*pid(j-(1:j)) )
      }
    }
  }
  undiag <- rep(NA,T)
  for(i in 1:T) undiag[i] <- sum(n[1:(i),(i+1):P]) + .5 * sum(n[1:i,i])
  undiag
}



