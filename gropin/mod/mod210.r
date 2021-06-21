############################# 
# start of Model script Gropin ID 210 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T,pH,aw,NaNO2)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(T,pH,aw,NaNO2) {
   mumax <-log(2)/exp(227.007-0.2721*T-377.389*aw-8.6224*pH+0.0303*NaNO2+0.00787*T*pH+3.2923*aw*pH-0.0117*aw*NaNO2-0.00249*pH*NaNO2+0.00273*(T^2)+171.9852*(aw^2)+0.3853*(pH^2))

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['pH'],argumentsPar['aw'],argumentsPar['NaNO2']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
