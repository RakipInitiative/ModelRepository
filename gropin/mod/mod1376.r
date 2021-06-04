############################# 
# start of Model script Gropin ID 1376 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(bw)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(bw) {
   mumax <-(2.652-3.023*bw+20.17*bw^2)

return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['bw']))
colnames(responseSurface) <- c(colnames(argumentsPar),'lnmumax')
#############################
# End of Model script
#############################
