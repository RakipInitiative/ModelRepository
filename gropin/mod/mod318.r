############################# 
# start of Model script Gropin ID 318 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T,pH,aw,Phe,NaNO2,CO2,Ac,La,Diac)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(T,pH,aw,Phe,NaNO2,CO2,Ac,La,Diac) {
   mumax <-0.419*(((T-(-2.83))^2)/((25-(-2.83))^2))*((aw-0.923)/(1-0.923))*(1-(10^(4.97-pH)))*(1-((La/(1+10^(pH-3.86)))/3.79))*((32-Phe)/32)*(((350-NaNO2)^2)/(350^2))*((3140-CO2)/3140)*(1-sqrt((Diac/(1+(10^(pH-4.8))))/4.8))*(1-sqrt((Ac/(1+(10^(pH-4.76))))/10.3))

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['pH'],argumentsPar['aw'],argumentsPar['Phe'],argumentsPar['NaNO2'],argumentsPar['CO2'],argumentsPar['Ac'],argumentsPar['La'],argumentsPar['Diac']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
