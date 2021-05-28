############################# 
# start of Model script Gropin ID 371 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T) {
   mumax <-(5.9*(10^-1)*(T-32.4)*((T+10.5)^2))/((25.3+10.5)*((25.3+10.5)*(T-25.3)-(25.3-32.4)*(25.3-10.5-2*T)))

return(mumax=mumax)
} 

# output parameters
mumax <- cbind(argumentsPar,response_surface(argumentsPar['T']))
colnames(mumax) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
