############################# 
# start of Model script Gropin ID 203 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T,aw,pH,NaNO2)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(T,aw,pH,NaNO2) {
   mumax <-252.833+0.1418*T-358.214*aw-18.4395*pH+0.0151*NaNO2-0.3653*T*aw+0.00452*T*pH+0.0000169*T*NaNO2+11.8359*aw*pH+0.00437*aw*NaNO2-0.00269*pH*NaNO2+0.00201*(T^2)+132.4864*(aw^2)+0.4881*(pH^2)+0.0000005*(NaNO2^2)

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['aw'],argumentsPar['pH'],argumentsPar['NaNO2']))
colnames(responseSurface) <- c(colnames(argumentsPar),'lnmumax')
#############################
# End of Model script
#############################
