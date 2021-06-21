############################# 
# start of Model script Gropin ID 328 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T,pH,aw)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(T,pH,aw) {
   mumax <-4.5*(((T-45.6)*((T-4.7)^2))/((40.6-4.7)*((40.6-4.7)*(T-40.6)-(40.6-45.6)*(40.6+4.7-2*T))))*(((pH-9.33)*(pH-3.85))/((6.59-3.85)*(pH-6.59)-(6.59-9.33)*(3.85-pH)))*(((aw-1)*((aw-0.944)^2))/((0.997-0.944)*((0.997-0.944)*(aw-0.997)-(0.997-1)*(0.997+0.944-2*aw))))

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['pH'],argumentsPar['aw']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
