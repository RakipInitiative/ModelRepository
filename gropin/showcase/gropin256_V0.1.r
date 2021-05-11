T <- seq(10,34,length.out=21)
a <- 0.0442
Tmin <- 5.407
visVar1 <- 'T'
visVar2 <- 'NA'
mode <- 'responsesurface'
lagTime <- 10
logIncrease <- 1
logN0 <- 0
logNEnd <- 10
T_kinetic <- 22
simTime <- 50
#############################
# start of Model script
#############################

response_surface <- function(T) {
  (a*(T-Tmin))^2
} 
mumax <- response_surface(T)
time2Xlog <- lagTime + logIncrease/mumax

q0 <- 1/(exp(lagTime)-1)
timesteps <- seq(0,simTime,length.out = 71)

logN_kinetic <- function(mumax) {
  A <- timesteps + (1/mumax)*log((exp(-mumax*timesteps)+q0)/(1+q0))
  logN = logN0 + mumax*A - log(1+((exp(mumax*A)-1)/(exp(logNEnd-logN0))))
  return(logN=logN)
}


timeTotal <- rep(timesteps,times=length(mumax))
TempTotal <-rep(T,each=length(timesteps))
mumaxTotal <-rep(mumax,each=length(timesteps))
logNTotal <- unlist(lapply(mumax, logN_kinetic))

resultTotal <- as.data.frame(cbind(timeTotal,mumaxTotal,logNTotal,TempTotal))
colnames(resultTotal) <- c('time in h',
                           'growthrate in days, polynomial eq.',
                           'log N in CFU/g',
                           'Temperature in C')


# output:
## time2Xlog
## t,logN
## responsesurface
#############################
# End of Model script
#############################

#############################
# start of Visualisation script
#############################



if(mode=='responsesurface') {
  plot(T,mumax,xlab='T',ylab='mu_max',
       main='Response surface mu_max for Gropin Model (ID 256)\n
        Staphylococcus aureus in/on Fresh milk')
}

if(mode=='time2multiply') {
  plot(T,time2Xlog,xlab='T',ylab='time in hours',
       main='Time in h to increase log step for Gropin Model (ID 256)\n
        Staphylococcus aureus in/on Fresh milk')
}

if(mode=='kinetic') {
  # extracting subset chosen by PAR_kinetic
  # for visualization
  subset_logN <- logN_kinetic(response_surface(T_kinetic))
  
  
  # plotting function
  plot(timesteps,subset_logN,xlab='t in h',ylab='log N in CFU/g',
       main='Growth prediction of Gropin Model (ID 256)\n
        Staphylococcus aureus in/on Fresh milk')
  newText <- paste0('parameters: T = ',T_kinetic)
  text(simTime*0.75,subset_logN[1],newText)
}
#############################
# End of Visualisation script
#############################