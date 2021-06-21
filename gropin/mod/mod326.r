############################# 
# start of Model script Gropin ID 326 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T,pH,aw)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(T,pH,aw) {
   mumax <-4.5*(((T-45.4)*((T-5)^2))/((40.9-5)*((40.9-5)*(T-40.9)-(40.9-45.4)*(40.9+5-2*T))))*(((pH-8.91)*(pH-3.88))/((6.4-3.88)*(pH-6.4)-(6.4-8.91)*(3.88-pH)))*(((aw-1)*((aw-0.944)^2))/((0.996-0.944)*((0.996-0.944)*(aw-0.996)-(0.996-1)*(0.996+0.944-2*aw))))

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['pH'],argumentsPar['aw']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
