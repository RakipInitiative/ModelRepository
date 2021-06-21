############################# 
# start of Model script Gropin ID 478 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(aw,NO2)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(aw,NO2) {
   mumax <-1150.5+(-2330.2)*aw+(-0.58142)*((NO2*1000)/(69.01*(1+10^(6.1-3.37))))+0.33406*aw*((NO2*1000)/(69.01*(1+10^(6.1-3.37))))+1182.1*(aw^2)+(-0.019875)*(((NO2*1000)/(69.01*(1+10^(6.1-3.37))))^2)

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['aw'],argumentsPar['NO2']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
