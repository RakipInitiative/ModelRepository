############################# 
# start of Model script Gropin ID 1270 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T,pH,Hours)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(T,pH,Hours) {
   mumax <-13.616*(-1)+0.203*T+3.0026*pH+8.42249*((17.4+0.0311*Hours+0.001036*T*Hours)/100)-0.00629*T*pH+0.11391*T*((17.4+0.0311*Hours+0.001036*T*Hours)/100)-0.00217*T^2-0.2171*pH^2-93.4381*((17.4+0.0311*Hours+0.001036*T*Hours)/100)^2

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['pH'],argumentsPar['Hours']))
colnames(responseSurface) <- c(colnames(argumentsPar),'lnmumax')
#############################
# End of Model script
#############################
