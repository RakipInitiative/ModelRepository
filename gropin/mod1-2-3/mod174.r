############################# 
# start of Model script Gropin ID 174 
#############################
 
# constant coefficients for this model
b13 <- 0.0001263
Tmin <- -3.27
pHmin <- 4.26
pHmax <- 9.77
 
variables <- data.frame(T,pH)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T,pH) {
   mumax <-((-b13)*((T-Tmin)^2)*(pH-pHmin)*(pH-pHmax))

return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['pH']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
