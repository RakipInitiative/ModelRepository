############################# 
# start of Model script Gropin ID 493 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T,days)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T,days) {
   mumax <-0.247+0.297*days+0.363*T-0.0075*(days^2)-0.00832*(T^2)-0.000796*T*days

return(mumax=mumax)
} 

# output parameters
mumax <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['days']))
colnames(mumax) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
