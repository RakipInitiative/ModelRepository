############################# 
# start of Model script Gropin ID 233 
#############################
 
# constant coefficients for this model
pHmin <- 4.2
pHmax <- 9.8
b2 <- 4.01
c1 <- 0.032
 
variables <- data.frame(pH)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(pH) {
   mumax <-b2*((pH-pHmin)*(1-exp(c1*(pH-pHmax))))^2

return(mumax=mumax)
} 

# output parameters
mumax <- cbind(argumentsPar,response_surface(argumentsPar['pH']))
colnames(mumax) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
