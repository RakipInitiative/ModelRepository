############################# 
# start of Model script Gropin ID 39 
#############################
 
# constant coefficients for this model
a0 <- -17.85
a1 <- 0.120318
a2 <- 4.1780189999999999
a3 <- 7.628412
a4 <- -0.30576
a5 <- -51.0245
 
variables <- data.frame(T,pH,aw)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(T,pH,aw) {
   mumax <-(a0+(a1*T)+(a2*pH)+(a3*aw)+(a4*(pH^2))+(a5*(aw^2)))

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['pH'],argumentsPar['aw']))
colnames(responseSurface) <- c(colnames(argumentsPar),'lnmumax')
#############################
# End of Model script
#############################
