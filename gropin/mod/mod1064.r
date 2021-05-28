############################# 
# start of Model script Gropin ID 1064 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(Sugar,pH)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(Sugar,pH) {
   mumax <-4655.09141-141.06579*Sugar+1.23229*(Sugar^2)-89.94556*pH+71.96275*(pH^2)-5.50379*Sugar*pH

return(mumax=mumax)
} 

# output parameters
mumax <- cbind(argumentsPar,response_surface(argumentsPar['Sugar'],argumentsPar['pH']))
colnames(mumax) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
