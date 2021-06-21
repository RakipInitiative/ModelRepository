############################# 
# start of Model script Gropin ID 833 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T,aw)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(T,aw) {
   mumax <-26.37*((((T-9.11)^2)*(T-46.45))/((32.11-9.11)*((32.11-9.11)*(T-32.11)-(32.11-46.45)*(32.11+9.11-2*T))))*((((aw-0.893)^2)*(aw-0.993))/((0.985-0.893)*((0.985-0.893)*(aw-0.985)-(0.985-0.993)*(0.985+0.893-2*aw))))

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['aw']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
