############################# 
# start of Model script Gropin ID 243 
#############################
 
# constant coefficients for this model
mopt_days^-1_ <- 21.89
awmin <- 0.906
pHmin <- -101.1
pHmax <- 5
pHopt <- 4.81
Tmax_oC_ <- 46.19
Tmin_oC_ <- 14.61
Topt_oC_ <- 34.36
 
variables <- data.frame(T,pH,aw)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T,pH,aw) {
   mumax <-mopt_days^-1_*((aw-awmin)/(1-awmin))*((((T-Tmin_oC_)^2)*(T-Tmax_oC_))/((Topt_oC_-Tmin_oC_)*((Topt_oC_-Tmin_oC_)*(T-Topt_oC_)-(Topt_oC_-Tmax_oC_)*(Topt_oC_+Tmin_oC_-2*T))))*((((pH-pHmin)^2)*(pH-pHmax))/((pHopt-pHmin)*((pHopt-pHmin)*(pH-pHopt)-(pHopt-pHmax)*(pHopt+pHmin-2*pH))))

return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['pH'],argumentsPar['aw']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
