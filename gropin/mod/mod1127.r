############################# 
# start of Model script Gropin ID 1127 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T,PL_SDAmix)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T,PL_SDAmix) {
   mumax <-(-0.1831)+0.0172*T-0.0231*PL_SDAmix-0.0046*T*PL_SDAmix+0.0009*(T^2)+0.0126*(PL_SDAmix^2)

return(mumax=mumax)
} 

# output parameters
mumax <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['PL_SDAmix']))
colnames(mumax) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
