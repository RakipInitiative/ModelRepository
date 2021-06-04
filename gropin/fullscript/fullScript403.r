#############################
# start of Parameter script
#############################
T <- seq(-1.8018,36.2637362637363,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 403 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T) {
   mumax <-(5.1*(10^-1)*(T-36.3)*((T+1.8)^2))/((27.8+1.8)*((27.8+1.8)*(T-27.8)-(27.8-36.3)*(27.8-1.8-2*T)))

return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 403 
#############################
plot(T,responseSurface$'mumax',xlab='T',
                          ylab='mumax',main='Response surface mumax for
Pseudomonas syringae in/on Nutrient broth
(gropin ID:403)')
#############################
# End of Visualisation script
#############################
