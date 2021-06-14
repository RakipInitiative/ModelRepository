############################# 
# start of Model script Gropin ID 1050 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(pH,S)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(pH,S) {
   mumax <-0.48*(pH-2.3)*(pH-15.1)/((pH-2.3)*(pH-15.1)-(pH-4)^2)*(1-S/981)

return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['pH'],argumentsPar['S']))
colnames(responseSurface) <- c(colnames(argumentsPar),'lnmumax')
#############################
# End of Model script
#############################
