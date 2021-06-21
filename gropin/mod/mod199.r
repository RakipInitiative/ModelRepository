############################# 
# start of Model script Gropin ID 199 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T,pH,NaNO2,aw)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(T,pH,NaNO2,aw) {
   mumax <-(-80.0191-(0.0709*T)+(255.1554*aw)-(7.3381*pH)+(0.0198*NaNO2)-(0.1321*T*aw)-(0.00315*T*pH)+(0.0000161*T*NaNO2)+(3.0076*aw*pH)+(0.00477*aw*NaNO2)-(0.00321*pH*NaNO2)+(0.00312*(T^2))-(155.3701*(aw^2))+(0.3088*(pH^2))-(0.0000005*(NaNO2^2)))

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['pH'],argumentsPar['NaNO2'],argumentsPar['aw']))
colnames(responseSurface) <- c(colnames(argumentsPar),'lnmumax')
#############################
# End of Model script
#############################
