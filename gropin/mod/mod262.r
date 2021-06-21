############################# 
# start of Model script Gropin ID 262 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T,pH,Ac)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(T,pH,Ac) {
   mumax <-(2.035+(0.818*T)+((-6.917)*(Ac/(1+10^(pH-4.76))))+(0.0009*(T^2))+(0.358*((Ac/(1+10^(pH-4.76)))^2))+((-0.196)*T*pH)+(1.259*pH*(Ac/(1+10^(pH-4.76)))))^2/24

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['pH'],argumentsPar['Ac']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
