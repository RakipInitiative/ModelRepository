#############################
# start of Parameter script
#############################
T <- seq(4.004,36.963036963037,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 1192 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T) {
   mumax <-0.025*(T-3.260)

return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T']))
colnames(responseSurface) <- c(colnames(argumentsPar),'Sqrmumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 1192 
#############################
plot(T,responseSurface$'Sqrmumax',xlab='T',
                          ylab='Sqrmumax',main='Response surface Sqrmumax for
Staphylococcus aureus in/on Eggs _grilled_
(gropin ID:1192)')
#############################
# End of Visualisation script
#############################
