lagtime <- 10
logIncrease <- 1
#responseSurface <- data.frame(cbind(seq(1,10),seq(1,10)))
responseSurface <- data.frame(cbind(rep(5:8,4),rep(1:4,each=4),seq(1,16)))

#############################
# start of Model script
#############################
library(reshape2)
myDF <- dim(responseSurface)

# mumax is always in the last column
mumax <- responseSurface[,myDF[2]]

# all corresponding parameters are in the other columns
nrOfIndependentVars <- myDF[2]-1
argumentsPar <- responseSurface[,1:nrOfIndependentVars]
myAxis <- colnames(argumentsPar)

timeToXlog <- lagtime + logIncrease/sqrt(mumax)
if(nrOfIndependentVars==2) {
  myDF <- data.frame(cbind(argumentsPar,timeToXlog))
  result <- acast(myDF,paste(myAxis[1], myAxis[2], sep="~"))
}


# output:
## time2Xlog
#############################
# End of Model script
#############################

#############################
# start of Visualisation script
#############################
mainTitleOfImage <- 'Time in h to increase log step'

if(nrOfIndependentVars==1) {
  plot(responseSurface[,1],timeToXlog,
       xlab=myAxis[1],ylab=myAxis[2],
       main=mainTitleOfImage)
}

if(nrOfIndependentVars==2) {
  myZ <- paste('time to increase',logIncrease,'step(s)')
  
  persp(as.numeric(rownames(result)),
        as.numeric(colnames(result)),
        result,
        col = 'green',
        xlab=myAxis[1],
        ylab=myAxis[2],
        zlab=myZ,
        main=mainTitleOfImage,
        theta=125,phi=20,shade=0.25,ticktype = 'detailed')
}


#############################
# End of Visualisation script
#############################