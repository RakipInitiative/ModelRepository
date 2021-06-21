############################# 
# start of Model script Gropin ID 311 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T,pH,aw,Phe,NaNO2,CO2)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(T,pH,aw,Phe,NaNO2,CO2) {
   mumax <-0.742*(((T-45.5)*((T+1.72)^2))/((37+1.72)*((37+1.72)*(T-37)-(37-45.5)*(37-1.72-2*T))))*(((pH-9.61)*(pH-4.71))/(((7.1-4.71)*(pH-7.1))-((7.1-9.61)*(4.71-pH))))*((aw-0.913)/(0.997-0.913))*(1-(NaNO2/25))*(1-(Phe/31.9))*(1-(CO2/3.04))

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['pH'],argumentsPar['aw'],argumentsPar['Phe'],argumentsPar['NaNO2'],argumentsPar['CO2']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
