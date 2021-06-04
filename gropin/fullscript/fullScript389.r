#############################
# start of Parameter script
#############################
T <- seq(4.9049,47.4525474525475,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 389 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T) {
   mumax <-(2.3*(T-47.5)*((T-4.9)^2))/((41.3-4.9)*((41.3-4.9)*(T-41.3)-(41.3-47.5)*(41.3+4.9-2*T)))

return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 389 
#############################
plot(T,responseSurface$'mumax',xlab='T',
                          ylab='mumax',main='Response surface mumax for
Escherichia coli in/on Liquid culture medium
(gropin ID:389)')
#############################
# End of Visualisation script
#############################
