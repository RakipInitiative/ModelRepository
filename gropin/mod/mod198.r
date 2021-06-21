############################# 
# start of Model script Gropin ID 198 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T,pH,aw,NaNO2)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(T,pH,aw,NaNO2) {
   mumax <-log(2)/exp((-732.446-(0.175*T)+(1565.5724*aw)-(9.5432*pH)+(0.0292*NaNO2)+(0.0718*T*aw)-(0.0143*T*pH)+(0.000005*T*NaNO2)+(6.1934*aw*pH)-(0.00716*aw*NaNO2)-(0.00283*pH*NaNO2)+(0.00273*(T^2))-(819.613*(aw^2))+(0.2633*(pH^2))-(0.000001*(NaNO2^2))))

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['pH'],argumentsPar['aw'],argumentsPar['NaNO2']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
