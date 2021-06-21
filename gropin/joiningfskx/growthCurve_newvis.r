lagtime <- 10.0
N0 <- 1
NEnd <- 9
simTime <- 20
showNt <- 1
responseSurface <- data.frame(cbind(expand.grid(seq(1,4),seq(1,4)),mumax=seq(1,16)))
#############################
# start of Model script
#############################
timesteps <- seq(0,simTime,length.out = 71)
#timesteps <- seq(0,simTime,by=0.5)

myDimDF <- dim(responseSurface)

# mumax is always in the last column
mumax <- responseSurface[,myDimDF[2]]
#mumax <- 0.03459

# all corresponding parameters are in the other columns
nrOfIndependentVars <- myDimDF[2]-1
argumentsPar <- responseSurface[,1:nrOfIndependentVars]
if(is.null(colnames(responseSurface))){
  myAxis <- paste('Parameter',1:nrOfIndependentVars)
} else {
  myAxis <- colnames(responseSurface)[1:nrOfIndependentVars]
  if(colnames(responseSurface)[nrOfIndependentVars+1]=='lnmumax') {
    mumax <- exp(mumax)
  }
  if(colnames(responseSurface)[nrOfIndependentVars+1]=='Sqrmumax') {
    mumax <- (mumax)^2
  }
}


kinetic <- function(mumax) {
  h0 <- lagtime*mumax
  Q <- rep(NA,length(timesteps))
  Q[1] <- log(1/(exp(h0)-1))
  
  N <- rep(NA,length(timesteps))
  N[1] <- N0
  for(ti in 2:length(timesteps)){
    rate <- mumax*(timesteps[ti]-timesteps[ti-1])
    Q[ti] <- Q[ti-1] + rate
    N[ti] <- min(N[ti-1] + 1/(1+exp(-Q[ti]))*(1-10^(-abs(N[ti-1]-NEnd)))*rate/log(10),NEnd)
  }
  return(N=N)
}

if(nrOfIndependentVars==1){
  resultTotal <- as.data.frame(cbind(rep(timesteps,times=length(mumax)),
                                     rep(mumax,each=length(timesteps)),
                                     unlist(lapply(mumax, kinetic)),
                                     rep(argumentsPar,each=length(timesteps))
  ))
}
if(nrOfIndependentVars>=2){
  resultTotal <- as.data.frame(cbind(rep(timesteps,times=length(mumax)),
                                     rep(mumax,each=length(timesteps)),
                                     unlist(lapply(mumax, kinetic)),
                                     matrix(unlist(rep(argumentsPar,each=length(timesteps))),nrow=71*myDimDF[1])
                                     #rep(argumentsPar[2],each=length(timesteps))
  ))
}
colnames(resultTotal) <- c('time in h',
                           'growthrate',
                           'N(t) in CFU/g',
                           myAxis)


#############################
# End of Model script
#############################

#############################
# start of Visualisation script for response surface data cube
#############################

# choose visual layer of datacube
visVar1 <- 'Var1' # example (choose according to variable in incoming secondary model)
visVar2 <- 'Var2' # example (choose according to variable in incoming secondary model)
myZlabel <- paste0('response surface ',colnames(responseSurface)[nrOfIndependentVars+1])
visAxes <- c(visVar1,visVar2)
expectedAxes <- colnames(responseSurface)[1:nrOfIndependentVars]
checkPresent<-match(expectedAxes,visAxes)
if (length(checkPresent[!is.na(checkPresent)])==0) {
  myErrMsg <- paste('axis names not found, please check cAsE senSiTiVe spelling. expected Axes:',
                    expectedAxes,
                    'and chosen axes are:',
                    visVar1,
                    ',',
                    visVar2)
  stop(myErrMsg)
}
missingAxis <- expectedAxes[is.na(checkPresent)]

if(length(checkPresent[!is.na(checkPresent)])==1) plot(argumentsPar[expectedAxes[checkPresent[!is.na(checkPresent)]]],mumax)
if(length(checkPresent[!is.na(checkPresent)])==2) {
  axis1 <- unlist(unique(argumentsPar[visVar1]))
  axis2 <- unlist(unique(argumentsPar[visVar2]))
  if(length(missingAxis)>0) {
    conditionForOtherAxis <- paste0(missingAxis,"==argumentsPar[1,'",missingAxis,"']",collapse = '&')
    result <- matrix(unlist(subset(responseSurface,eval(parse(text=conditionForOtherAxis)))[colnames(responseSurface)[nrOfIndependentVars+1]]),nrow=length(axis1))
  } else {
    result <- matrix(unlist(responseSurface[colnames(responseSurface)[nrOfIndependentVars+1]]),nrow=length(axis1))
  }
  persp(axis1,
        axis2, 
        result,
        col = 'green',
        xlab=visVar1,
        ylab=visVar2,
        zlab=myZlabel,
        main='Response surface for chosen secondary gropin model',
        sub='generic visualisation',
        theta=305,
        phi=20,
        shade=0.25,
        ticktype = 'detailed')
}




#############################
# End of Visualisation script
#############################
