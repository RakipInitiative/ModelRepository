############################# 
# start of Model script Gropin ID 200 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T,pH,NaCl,NaNO2)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(T,pH,NaCl,NaNO2) {
   mumax <-log(2)/exp(21.4583-0.268*T+0.0128*NaCl-5.2966*pH+0.0202*NaNO2-0.00000794*T*NaCl+0.00757*T*pH-0.00000051*T*NaNO2-0.00137*NaCl*pH+0.00000528*NaCl*NaNO2-0.00278*pH*NaNO2+0.00267*(T^2)+0.000122*(NaCl^2)+0.3842*(pH^2)-0.00000059*(NaNO2^2))

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['pH'],argumentsPar['NaCl'],argumentsPar['NaNO2']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
