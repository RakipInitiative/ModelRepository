############################# 
# start of Model script Gropin ID 307 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T,pH,aw)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(T,pH,aw) {
   mumax <-(1.9*((T-45.5)*((T+3)^2)/((37+3)*((37+3)*(T-37)-(37-45.5)*(37-3-2*2*T))))*(((pH-9.61)*(pH-4.38))/((7.1-4.38)*(pH-7.1)-(7.1-9.61)*(4.38-pH)))*(((aw-1)*((aw-0.913)^2))/((0.997-0.913)*((0.997-0.913)*(aw-0.997)-(0.997-1)*(0.997+0.913-2*aw)))))

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['pH'],argumentsPar['aw']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
