############################# 
# start of Model script Gropin ID 1378 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(bw)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(bw) {
   mumax <-(-0.4857+3.901*bw-23.78*bw^2)

return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['bw']))
colnames(responseSurface) <- c(colnames(argumentsPar),'lnmumax')
#############################
# End of Model script
#############################
