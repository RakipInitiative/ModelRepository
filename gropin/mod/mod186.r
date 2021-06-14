############################# 
# start of Model script Gropin ID 186 
#############################
 
# constant coefficients for this model
a1 <- 2.9465
a2 <- -0.3604
a3 <- -0.47420000000000001
a4 <- 5.6668
a5 <- 0.03049
a6 <- -0.6344
a7 <- -0.03586
a8 <- 0.0076
a9 <- -0.2589
 
variables <- data.frame(pH,T,CO2)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(pH,T,CO2) {
   mumax <-a1+(a2*pH)+(a3*T)+(a4*CO2)+(a5*pH*T)+(a6*pH*CO2)+(a7*T*CO2)+(a8*(T^2))+(a9*(CO2^2))

return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['pH'],argumentsPar['T'],argumentsPar['CO2']))
colnames(responseSurface) <- c(colnames(argumentsPar),'lnmumax')
#############################
# End of Model script
#############################
