############################# 
# start of Model script Gropin ID 187 
#############################
 
# constant coefficients for this model
a1 <- 4.8719
a2 <- 0.128
a3 <- -1.19333
a4 <- 10.1099
a5 <- -0.04453
a6 <- -1.41277
a7 <- -0.24792
a8 <- 0.08419
a9 <- 1.83963
 
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
