############################# 
# start of Model script Gropin ID 1129 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T,PL_SDAmix)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T,PL_SDAmix) {
   mumax <-0.1312-0.0322*T+0.0972*PL_SDAmix-0.0080*T*PL_SDAmix+0.0023*(T^2)-0.0076*(PL_SDAmix^2)

return(mumax=mumax)
} 

# output parameters
mumax <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['PL_SDAmix']))
colnames(mumax) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
