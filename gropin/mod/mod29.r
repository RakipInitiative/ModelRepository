############################# 
# start of Model script Gropin ID 29 
#############################
 
# constant coefficients for this model
Il <- -15000
I1 <- -44.7
I3 <- 32000
I4 <- -0.048
I6 <- -17000
I8 <- 44.9
 
variables <- data.frame(T,aw)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(T,aw) {
   mumax <-(Il+(I1*T)+(I3*aw)+(I4*(T^2))+(I6*(aw^2))+(I8*T*aw))

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['aw']))
colnames(responseSurface) <- c(colnames(argumentsPar),'Sqrmumax')
#############################
# End of Model script
#############################
