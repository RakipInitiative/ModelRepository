############################# 
# start of Model script Gropin ID 211 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(aw,pH,NaNO2,T)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(aw,pH,NaNO2,T) {
   mumax <-248.1902-342.9966*aw-18.9842*pH+0.0169*NaNO2-0.192*T*aw+0.0000215*T*NaNO2+12.5454*aw*pH-0.00231*pH*NaNO2+0.00203*(T^2)+121.0776*(aw^2)+0.4816*(pH^2)

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['aw'],argumentsPar['pH'],argumentsPar['NaNO2'],argumentsPar['T']))
colnames(responseSurface) <- c(colnames(argumentsPar),'lnmumax')
#############################
# End of Model script
#############################
