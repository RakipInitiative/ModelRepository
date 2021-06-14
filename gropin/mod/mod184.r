############################# 
# start of Model script Gropin ID 184 
#############################
 
# constant coefficients for this model
a1 <- 0.2513
a2 <- 0.2099
a3 <- 0.33350000000000002
a4 <- -1.672
a5 <- -0.0208
a6 <- 0.0867
a7 <- 0.1007
a8 <- -0.0143
a9 <- 0.1663
 
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
