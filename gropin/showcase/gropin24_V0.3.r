T <- seq(4,12,length.out=21)
aw <- seq(0.974,0.992,length.out=21)
CO2dissolved <- seq(0,2411,length.out=21)
b <- 1.17e-05
awmin <- 0.9715
Tmin <- -5.68
CO2max <- 2300
visVar1 <- 'T'
visVar2 <- 'aw'
mode <- 'responsesurface'
lagTime <- 10
logIncrease <- 1
logN0 <- 0
logNEnd <- 10
simTime <- 1000
T_kinetic <- 8
aw_kinetic <- 0.983
CO2dissolved_kinetic <- 1205.5
#############################
# start of Model script
#############################
# if user chooses both visual Axes the same, the code breaks
# this way a different visual Axis is chosen randomly if user does this
if(visVar1==visVar2){
  visVar2 <- sample(expectedAxes[which(expectedAxes!=visVar2)],1)
}
timesteps <- seq(0,simTime,length.out = 71)

response_surface <- function(T,aw,CO2dissolved) {
  (b*(aw-awmin)*(CO2max-CO2dissolved)*((T-Tmin)^2))
} 

arguments <- cbind.data.frame(T,aw,CO2dissolved)
argumentsPar <- expand.grid(T,aw,CO2dissolved)
colnames(argumentsPar) <- c("T","aw","CO2dissolved")

muMax <- response_surface(argumentsPar["T"],argumentsPar["aw"],argumentsPar["CO2dissolved"])

time2Xlog_val <- lagTime + logIncrease/muMax
time2Xlog <- cbind.data.frame(argumentsPar,time2Xlog_val)

q0 <- 1/(exp(lagTime)-1)
logN <- function(growthRate) {
  A <- timesteps + (1/growthRate)*log((exp(-growthRate*timesteps)+q0)/(1+q0))
  logN = logN0 + growthRate*A - log(1+((exp(growthRate*A)-1)/(exp(logNEnd-logN0))))
  return(logN=logN)
}

timeTotal <- rep(timesteps,times=length(unlist(muMax)))
TempTotal <-rep(unlist(argumentsPar["T"]),each=length(timesteps))
awTotal <- rep(unlist(argumentsPar["aw"]),each=length(timesteps))
CO2dissolvedTotal <- rep(unlist(argumentsPar["CO2dissolved"]),each=length(timesteps))
mumaxTotal <-rep(unlist(muMax),each=length(timesteps))
logNTotal <- unlist(lapply(unlist(muMax), logN))

resultTotal <- cbind.data.frame(timeTotal,mumaxTotal,logNTotal,TempTotal,awTotal,CO2dissolvedTotal)
colnames(resultTotal) <- c('time in h',
                           'growthrate in days, polynomial eq.',
                           'log N in CFU/g',
                           'Temperature in C',
                           'aw',
                           'CO2dissolved in ppm')
#############################
# End of Model script
#############################

#############################
# start of Visualisation script
#############################
# chosen visual layer
visAxes <- c(visVar1,visVar2)
expectedAxes <- c('T','aw','CO2dissolved')
checkPresent<-match(expectedAxes,visAxes)
missingAxis <- expectedAxes[is.na(checkPresent)]

if("T" %in% visAxes){
  ax1 <- T
} else {
  ax1 <- T_kinetic
}
if("aw" %in% visAxes){
  ax2 <- aw
} else {
  ax2 <- aw_kinetic
}
if("CO2dissolved" %in% visAxes){
  ax3 <- CO2dissolved
} else {
  ax3 <- CO2dissolved_kinetic
}

argumentsVisVar <- expand.grid(ax1,ax2,ax3)
colnames(argumentsVisVar) <- expectedAxes
firstChosenAxis <- expectedAxes[
  min(
    which(expectedAxes == expectedAxes[match(1,checkPresent)]),
    which(expectedAxes == expectedAxes[match(2,checkPresent)]))
]
secondChosenAxis <- expectedAxes[
  max(
    which(expectedAxes == expectedAxes[match(1,checkPresent)]),
    which(expectedAxes == expectedAxes[match(2,checkPresent)]))
]

result <- matrix(unlist(response_surface(argumentsVisVar["T"],
                                         argumentsVisVar["aw"],
                                         argumentsVisVar["CO2dissolved"])),
                 nrow=21,
                 byrow=F)
#order of entries in result is always: 
# rows of the 1st chosen index of expectedAxes
# cols of the 2nd chosen index of expectedAxes
# this makes sure, the order of visVar1 and visVar2 are not relevant
rownames(result) <- unlist(arguments[firstChosenAxis])
colnames(result) <- unlist(arguments[secondChosenAxis])

if(mode=='responsesurface') {
  persp(as.numeric(rownames(result)),
        as.numeric(colnames(result)),
        result,
        col = 'green',
        xlab=firstChosenAxis,
        ylab=secondChosenAxis,
        zlab='mu_max',
        main='Response surface mu_max for Gropin Model (ID 24)',
        sub='Aeromonas hydrophilia in/on modified BHI',
        theta=305,
        phi=20,
        shade=0.25,
        ticktype = 'detailed')
}
if(mode=='time2multiply') {
  time2XlogVis <- lagTime + logIncrease/result
  
  
  myZ <- paste('time to increase',logIncrease,' logstep(s)')
  
  persp(as.numeric(rownames(time2XlogVis)),
        as.numeric(colnames(time2XlogVis)),
        time2XlogVis,
        col = 'green',
        xlab=firstChosenAxis,
        ylab=secondChosenAxis,
        zlab=myZ,
        main='Time in h to increase log step for Gropin Model (ID 24)',
        sub='Aeromonas hydrophilia in/on modified BHI',
        theta=305,
        phi=20,
        shade=0.25,
        ticktype = 'detailed')
}
if(mode=='kinetic') {
  
  plot(timesteps,logN(response_surface(T_kinetic,aw_kinetic,CO2dissolved_kinetic)),
       xlab='t in h',
       ylab='log N in CFU/g',
       main='Growth prediction of Gropin Model (ID 24)\n
        Aeromonas hydrophilia in/on modified BHI')
  
}
#############################
# End of Visualisation script
#############################

