#############################
# start of Parameter script
#############################
T <- seq(-1.7982017982018,36.3363,length.out=21)
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
mumax <- cbind(argumentsPar,response_surface(argumentsPar['T']))
colnames(mumax) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 403 
#############################
plot(T,mumax$mumax,
                          xlab='T',
                          ylab='mu_max')
#############################
# End of Visualisation script
#############################
