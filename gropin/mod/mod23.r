#############################
# start of Model script
#############################
 
variables <- data.frame(T,aw)
argumentsPar <- expand.grid(variables)
response_surface <- function(T,aw) {
   mumax <-matrix(unlist((a*(aw-awmin)*((T-Tmin)^2)),nrow=21))
return(mumax=mumax)
} 
mumax <- response_surface(argumentsPar['T'],argumentsPar['aw'])
#############################
# End of Model script
#############################
