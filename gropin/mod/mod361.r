############################# 
# start of Model script Gropin ID 361 
#############################
 
# constant coefficients for this model
lagref <- 20.7
dlag <- 1.73
Elag <- 67
 
variables <- data.frame(T,pH)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(T,pH) {
   mumax <-(log(lagref)-dlag*(5.7-pH)-(Elag/0.00831)*((1/(T+273))-(1/273)))

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['pH']))
colnames(responseSurface) <- c(colnames(argumentsPar),'lnmumax')
#############################
# End of Model script
#############################
