#############################
# start of Parameter script
#############################
T <- seq(2.1021,42.5574425574426,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 402 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T) {
   mumax <-(6.4*(10^-3)*(T-42.6)*((T-2.1)^2))/((36.7-2.1)*((36.7-2.1)*(T-36.7)-(36.7-42.6)*(36.7+2.1-2*T)))

return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 402 
#############################
plot(T,responseSurface$'mumax',xlab='T',
                          ylab='mumax',main='Response surface mumax for
Pseudomonas morganii in/on Nutrient broth
(gropin ID:402)')
#############################
# End of Visualisation script
#############################
