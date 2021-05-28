############################# 
# start of Model script Gropin ID 49 
#############################
 
# constant coefficients for this model
a0 <- -10.05
a1 <- 0.34
a2 <- 1.3
a3 <- -0.0048
a4 <- 0.00072
a5 <- -0.1
 
variables <- data.frame(T,pH)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T,pH) {
   mumax <-(a0+(a1*T)+(a2*pH)+(a3*(T^2))+(a4*T*pH)+(a5*(pH^2)))

return(mumax=mumax)
} 

# output parameters
mumax <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['pH']))
colnames(mumax) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
