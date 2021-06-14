############################# 
# start of Model script Gropin ID 36 
#############################
 
# constant coefficients for this model
awmin <- 0.895
awopt <- 0.988
 
variables <- data.frame(aw)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(aw) {
   mumax <-(9.97^0.5)*(((aw-1)*(aw-awmin)^2)/((awopt-awmin)*(((awopt-awmin)*(aw-awopt))-((awopt-1)*(awopt+awmin-2*aw)))))^0.5

return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['aw']))
colnames(responseSurface) <- c(colnames(argumentsPar),'Sqrmumax')
#############################
# End of Model script
#############################
