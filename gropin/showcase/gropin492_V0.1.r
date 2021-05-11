mode <- 'responsesurface'
lagTime <- 10
logIncrease <- 1
logN0 <- 0
logNEnd <- 10
simTime <- 20
days_kinetic <- 14
T_kinetic <- 9.5
days <- seq(0,28,length.out=21)
T <- seq(4,15,length.out=21)
#############################
# start of Model script
#############################
multVar1 <- T
multVar2 <- days

response_surface <- function(T,days) {
  0.89+0.081*days+0.11*T+0.000192*(days^2)-0.0034*(T^2)+0.0068*T*days
} 
mumax <- outer(multVar1,multVar2,response_surface)
colnames(mumax)<-multVar2
rownames(mumax)<-multVar1

time2Xlog <- lagTime + logIncrease/mumax

q0 <- 1/(exp(lagTime)-1)

t <- seq(0,simTime,length.out = 71)
logN <- function(mumax) {
  A <- t + (1/mumax)*log((exp(-mumax*t)+q0)/(1+q0))
  logN = logN0 + mumax*A - log(1+((exp(mumax*A)-1)/(exp(logNEnd-logN0))))
  return(logN=logN)
}


timeTotal <- rep(t,times=length(mumax))
TempTotal <-rep(rep(T,each=length(t)),times=length(days))
daysTotal <- rep(rep(days,each=length(t)),each=length(T))
mumaxTotal <-rep(mumax,each=length(t))
logNTotal <- unlist(lapply(mumax, logN))

resultTotal <- as.data.frame(cbind(timeTotal,mumaxTotal,logNTotal,TempTotal,daysTotal))
colnames(resultTotal) <- c('time in h',
                           'growthrate in days, polynomial eq.',
                           'log N in CFU/g',
                           'Temperature in C',
                           'days in days')
# output:
## time2Xlog
## t,logN, corresponding parameters as bound df
## responsesurface

#############################
# End of Model script
#############################
#############################
# start of Visualisation script
#############################
if(mode=='responsesurface') {
  persp(multVar1,
        multVar2,
        mumax,
        col = 'green',
        xlab='T',ylab='days',
        zlab='mu_max',
        main='Response surface mu_max for Gropin Model (ID 492)\n
      Listeria monocytogenes in/on Cheese Milk',
        theta=35,
        phi=20,
        shade=0.25,
        ticktype = 'detailed')
}

if(mode=='time2multiply') {
  
  myZ <- paste('time to increase',logIncrease,'step(s)')
  
  persp(multVar1,
        multVar2,
        time2Xlog,
        col = 'green',
        xlab='T',
        ylab='days',
        zlab=myZ,
        main='Time in h to increase log step for Gropin Model (ID 492)\n
       Listeria monocytogenes in/on Cheese Milk',
        theta=125,phi=20,shade=0.25,ticktype = 'detailed')
}

if(mode=='kinetic') {
  mumax_kinetic <- mumax[as.character(T_kinetic),as.character(days_kinetic)]
  parText <- paste0('Temperature = ',
                    T_kinetic,
                    ',\ndays = ', days_kinetic)
  plot(t,logN(mumax_kinetic),
       xlab='t in h',
       ylab='log N in CFU/g',
       main='Growth prediction of Gropin Model (ID 492)\n
       Listeria monocytogenes in/on Cheese Milk')
  text(simTime*0.75,logN(mumax_kinetic)[1],parText)
  
}
#############################
# End of Visualisation script
#############################
