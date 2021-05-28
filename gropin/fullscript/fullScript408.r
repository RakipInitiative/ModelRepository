#############################
# start of Parameter script
#############################
T <- seq(-3.4965034965035,29.4294,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 408 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T) {
   mumax <-(4.7*(10^-3)*(T-29.4)*((T+3.5)^2))/((26+3.5)*((26+3.5)*(T-26)-(26-29.4)*(26-3.5-2*T)))

return(mumax=mumax)
} 

# output parameters
mumax <- cbind(argumentsPar,response_surface(argumentsPar['T']))
colnames(mumax) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 408 
#############################
plot(T,mumax$mumax,
                          xlab='T',
                          ylab='mu_max')
#############################
# End of Visualisation script
#############################
