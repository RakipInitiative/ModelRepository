############################# 
# start of Model script Gropin ID 1353 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(bw)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(bw) {
   mumax <-(2.351-2.531*bw+22.02*bw^2)

return(mumax=mumax)
} 

# output parameters
mumax <- cbind(argumentsPar,response_surface(argumentsPar['bw']))
colnames(mumax) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
