############################# 
# start of Model script Gropin ID 25 
#############################
 
# constant coefficients for this model
Im <- 89.6
m1 <- -0.8
m3 <- -181
m4 <- 0.00013
m6 <- 92.1
m8 <- 0.82
 
variables <- data.frame(T,aw)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(T,aw) {
   mumax <-(Im+(m1*T)+(m3*aw)+(m4*(T^2))+(m6*(aw^2))+(m8*T*aw))

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['aw']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
