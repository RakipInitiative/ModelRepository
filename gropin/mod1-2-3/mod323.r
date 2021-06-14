############################# 
# start of Model script Gropin ID 323 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T,pH,aw)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T,pH,aw) {
   mumax <-4.247*(((T-45.16)*((T-6.81)^2))/(34.16*(34.16*(T-40.97)-(-4.19)*(47.78-2*T))))*(((pH-8.98)*(pH-4))/(2.49*(pH-6.49)-(-2.49)*(4-pH)))*(((aw-1)*((aw-0.941)^2))/(0.057*(0.057*(aw-0.998)-(-0.002)*(1.939-2*aw))))

return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['pH'],argumentsPar['aw']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
