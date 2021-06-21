############################# 
# start of Model script Gropin ID 317 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T,pH,aw,Phe,NaNO2,CO2,UAc,ULa,UDiac)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(T,pH,aw,Phe,NaNO2,CO2,UAc,ULa,UDiac) {
   mumax <-0.419*(((T-(-2.83))^2)/((25-(-2.83))^2))*((aw-0.923)/(1-0.923))*(1-(10^(4.97-pH)))*(1-(ULa/3.79))*((32-Phe)/32)*(((350-NaNO2)/350)^2)*((3140-CO2)/3140)*(1-sqrt(UDiac/4.8))*(1-sqrt(UAc/10.3))

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['pH'],argumentsPar['aw'],argumentsPar['Phe'],argumentsPar['NaNO2'],argumentsPar['CO2'],argumentsPar['UAc'],argumentsPar['ULa'],argumentsPar['UDiac']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
