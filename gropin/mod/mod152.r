############################# 
# start of Model script Gropin ID 152 
#############################
 
# constant coefficients for this model
c <- 0.012
awmin <- 0.9469
Tmin <- -2.31
 
variables <- data.frame(T,aw)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T,aw) {
   mumax <-(1/(c*(aw-awmin)*((T-Tmin)^2)))

return(mumax=mumax)
} 

# output parameters
mumax <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['aw']))
colnames(mumax) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
