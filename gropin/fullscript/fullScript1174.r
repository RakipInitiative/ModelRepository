#############################
# start of Parameter script
#############################
T <- seq(4.6046,45.954045954046,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 1174 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T) {
   mumax <-0.0046*((T-8.67)^2)*(1-exp(0.0656*(T-49.68)))

return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 1174 
#############################
plot(T,responseSurface$'mumax',xlab='T',
                          ylab='mumax',main='Response surface mumax for
Bacillus cereus in/on Rice _cooling of cooked_
(gropin ID:1174)')
#############################
# End of Visualisation script
#############################
