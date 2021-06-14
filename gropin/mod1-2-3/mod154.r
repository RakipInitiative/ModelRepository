############################# 
# start of Model script Gropin ID 154 
#############################
 
# constant coefficients for this model
l <- 6.04e_08
l1 <- -504
l2 <- 0.16
l3 <- -5700
l4 <- 0.83
l5 <- 3.28e-06
l6 <- -6.04e_08
l7 <- -0.00107
l8 <- 495
l9 <- -0.16
 
variables <- data.frame(T,aw,CO2_dissolved_)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T,aw,CO2_dissolved_) {
   mumax <-(l+(l1*T)+(l2*CO2_dissolved_)+(l3*aw)+(l4*(T^2))+(l5*(CO2_dissolved_^2))+(l6*(aw^2))+(l7*T*CO2_dissolved_)+(l8*T*aw)+(l9*CO2_dissolved_*aw))

return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['aw'],argumentsPar['CO2_dissolved_']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
