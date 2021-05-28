############################# 
# start of Model script Gropin ID 169 
#############################
 
# constant coefficients for this model
awmin <- 0.856
awopt <- 0.981
 
variables <- data.frame(aw)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(aw) {
   mumax <-(2.86^0.5)*(((aw-1)*(aw-awmin)^2)/((awopt-awmin)*(((awopt-awmin)*(aw-awopt))-((awopt-1)*(awopt+awmin-2*aw)))))^0.5

return(mumax=mumax)
} 

# output parameters
mumax <- cbind(argumentsPar,response_surface(argumentsPar['aw']))
colnames(mumax) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
