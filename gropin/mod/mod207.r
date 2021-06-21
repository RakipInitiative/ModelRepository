############################# 
# start of Model script Gropin ID 207 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(pH,NaNO2,T,aw)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(pH,NaNO2,T,aw) {
   mumax <-34.3435-4.4105*pH+0.0277*NaNO2-0.2272*T*aw-0.00366*pH*NaNO2+0.00319*(T^2)-13.9892*(aw^2)+0.3089*(pH^2)

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['pH'],argumentsPar['NaNO2'],argumentsPar['T'],argumentsPar['aw']))
colnames(responseSurface) <- c(colnames(argumentsPar),'lnmumax')
#############################
# End of Model script
#############################
