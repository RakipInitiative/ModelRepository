############################# 
# start of Model script Gropin ID 23 
#############################
 
# constant coefficients for this model
a <- 0.032
awmin <- 0.9718
Tmin <- -5.25
 
variables <- data.frame(T,aw)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T,aw) {
   mumax <-(a*(aw-awmin)*((T-Tmin)^2))

return(mumax=mumax)
} 

# output parameters
mumax <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['aw']))
colnames(mumax) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
