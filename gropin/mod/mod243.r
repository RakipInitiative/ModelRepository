############################# 
# start of Model script Gropin ID 243 
#############################
 
# constant coefficients for this model
moptdays1 <- 21.89
awmin <- 0.906
pHmin <- -101.1
pHmax <- 5
pHopt <- 4.81
TmaxoC <- 46.19
TminoC <- 14.61
ToptoC <- 34.36
 
variables <- data.frame(T,pH,aw)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T,pH,aw) {
   mumax <-moptdays1*((aw-awmin)/(1-awmin))*((((T-TminoC)^2)*(T-TmaxoC))/((ToptoC-TminoC)*((ToptoC-TminoC)*(T-ToptoC)-(ToptoC-TmaxoC)*(ToptoC+TminoC-2*T))))*((((pH-pHmin)^2)*(pH-pHmax))/((pHopt-pHmin)*((pHopt-pHmin)*(pH-pHopt)-(pHopt-pHmax)*(pHopt+pHmin-2*pH))))

return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['pH'],argumentsPar['aw']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
