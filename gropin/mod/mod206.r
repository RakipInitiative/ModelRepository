############################# 
# start of Model script Gropin ID 206 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T,pH,aw,NaNO2)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(T,pH,aw,NaNO2) {
   mumax <-log(2)/exp(37.0474-0.1096*T-9.0923*pH+0.0232*NaNO2-0.0133*T*pH+5.7907*aw*pH-0.00296*pH*NaNO2+0.00271*(T^2)-23.5166*(aw^2)+0.2573*(pH^2)-0.00000085*(NaNO2^2))

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['pH'],argumentsPar['aw'],argumentsPar['NaNO2']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
