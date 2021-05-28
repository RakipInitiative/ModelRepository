############################# 
# start of Model script Gropin ID 1077 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T,Limonin)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T,Limonin) {
   mumax <-7.26-0.386*T+0.006*(T^2)-0.257*Limonin+0.008*T*Limonin

return(mumax=mumax)
} 

# output parameters
mumax <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['Limonin']))
colnames(mumax) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
