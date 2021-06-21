############################# 
# start of Model script Gropin ID 202 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T,pH,aw,NaNO2)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(T,pH,aw,NaNO2) {
   mumax <-log(2)/exp(227.7984-0.2465*T-380.8103*aw-8.4117*pH+0.0308*NaNO2-0.0287*T*aw+0.00829*T*pH-0.0000025*T*NaNO2+3.0406*aw*pH-0.0111*aw*NaNO2-0.00268*pH*NaNO2+0.00274*(T^2)+174.7631*(aw^2)+0.3882*(pH^2)+0.0000003*(NaNO2^2))

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['pH'],argumentsPar['aw'],argumentsPar['NaNO2']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
