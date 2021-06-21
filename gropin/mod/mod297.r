############################# 
# start of Model script Gropin ID 297 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T,pH,aw,NaNO2)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(T,pH,aw,NaNO2) {
   mumax <-log(2)/exp(13.5303-0.1096*T+0.0266*(-33.3333+0.0220472*((sqrt(1.51165*(10^21)*(aw^2)-3.28336*(10^21)*aw+1.78325*(10^21))-3.888*(10^10)*aw+4.22242*(10^10))^(1/3))-15712.5/((sqrt(1.51165*(10^21)*(aw^2)-3.28336*(10^21)*aw+1.78325*(10^21))-3.888*(10^10)*aw+4.22242*(10^10))^(1/3)))-3.2997*pH+0.0232*NaNO2-0.0134*T*pH-0.00332*(-33.3333+0.0220472*((sqrt(1.51165*(10^21)*(aw^2)-3.28336*(10^21)*aw+1.78325*(10^21))-3.888*(10^10)*aw+4.22242*(10^10))^(1/3))-15712.5/((sqrt(1.51165*(10^21)*(aw^2)-3.28336*(10^21)*aw+1.78325*(10^21))-3.888*(10^10)*aw+4.22242*(10^10))^(1/3)))*pH-0.00296*pH*NaNO2+0.00271*(T^2)+0.257*(pH^2)-0.00000085*(NaNO2^2))

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['pH'],argumentsPar['aw'],argumentsPar['NaNO2']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
