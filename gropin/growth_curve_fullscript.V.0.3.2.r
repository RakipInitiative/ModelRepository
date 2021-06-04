lagtime <- 10.0
N0 <- 1
NEnd <- 9
simTime <- 20
showNt <- 12
#responseSurface <- data.frame(cbind(seq(1,10),seq(1,10)))
responseSurface <- data.frame(cbind(rep(5:8,4),rep(1:4,each=4),seq(1,16)))
#colnames(responseSurface) <- c("myPar1", "myPar2",'lnmumax')
colnames(responseSurface) <- c("myPar1", "myPar2",'mumax')
colnames(responseSurface) <- c("myPar1", "myPar2",'Sqrmumax')
#############################
# start of Model script
#############################
timesteps <- seq(0,simTime,length.out = 71)
#timesteps <- seq(0,simTime,by=0.5)

myDF <- dim(responseSurface)

# mumax is always in the last column
mumax <- responseSurface[,myDF[2]]

# all corresponding parameters are in the other columns
nrOfIndependentVars <- myDF[2]-1
argumentsPar <- responseSurface[,1:nrOfIndependentVars]
if(is.null(colnames(responseSurface))){
  myAxis <- paste('Parameter',1:nrOfIndependentVars)
} else {
  myAxis <- colnames(responseSurface)[1:nrOfIndependentVars]
  #if(myAxis[length(myAxis)]=='<mu>max') {}
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
if(nrOfIndependentVars==2){
  resultTotal <- as.data.frame(cbind(rep(timesteps,times=length(mumax)),
                                     rep(mumax,each=length(timesteps)),
                                     unlist(lapply(mumax, kinetic)),
                                     rep(argumentsPar[1],each=length(timesteps)),
                                     rep(argumentsPar[2],each=length(timesteps))
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
# start of Visualisation script
#############################
# extracting subset chosen by showNt
# for visualization
mainText <- paste0('Growth prediction of Gropin Model(',
                   colnames(responseSurface)[nrOfIndependentVars+1],
                   ')')
plot(timesteps,unlist(lapply(mumax, kinetic)[showNt]),
     xlab='time in h',
     ylab='N(t) in CFU/g',
     main=mainText)
if(nrOfIndependentVars==1){
  newText <- paste0(myAxis,' = ',argumentsPar[showNt],collapse='\n')
}
if(nrOfIndependentVars==2){
  newText <- paste0(myAxis,' = ',argumentsPar[showNt,],collapse='\n')
}
text(simTime*0.75,1,newText)
#############################
# End of Visualisation script
#############################