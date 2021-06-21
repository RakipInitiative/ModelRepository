############################# 
# start of Model script Gropin ID 298 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T,pH,aw,NaNO2)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(T,pH,aw,NaNO2) {
   mumax <-log(2)/exp(21.2574-0.2643*T+0.00404*(-33.3333+0.0220472*((sqrt(1.51165*(10^21)*(aw^2)-3.28336*(10^21)*aw+1.78325*(10^21))-3.888*(10^10)*aw+4.22242*(10^10))^(1/3))-15712.5/((sqrt(1.51165*(10^21)*(aw^2)-3.28336*(10^21)*aw+1.78325*(10^21))-3.888*(10^10)*aw+4.22242*(10^10))^(1/3)))-5.2054*pH+0.0189*NaNO2+0.00709*T*pH-0.00252*pH*NaNO2+0.00265*(T^2)+0.000129*((-33.3333+0.0220472*((sqrt(1.51165*(10^21)*(aw^2)-3.28336*(10^21)*aw+1.78325*(10^21))-3.888*(10^10)*aw+4.22242*(10^10))^(1/3))-15712.5/((sqrt(1.51165*(10^21)*(aw^2)-3.28336*(10^21)*aw+1.78325*(10^21))-3.888*(10^10)*aw+4.22242*(10^10))^(1/3)))^2)+0.3746*(pH^2))

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['pH'],argumentsPar['aw'],argumentsPar['NaNO2']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
