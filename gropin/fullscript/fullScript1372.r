#############################
# start of Parameter script
#############################
bw <- seq(0.032032,0.446753246753247,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 1372 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(bw)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(bw) {
   mumax <-(2.682-5.124*bw+26.62*bw^2)

return(mumax=mumax)
} 

# output parameters
mumax <- cbind(argumentsPar,response_surface(argumentsPar['bw']))
colnames(mumax) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 1372 
#############################
plot(bw,mumax$mumax,
                          xlab='bw',
                          ylab='mu_max',main='Response surface mu_max for
Aspergillus oryzae in/on Basal medium
(gropin ID:1372)')
#############################
# End of Visualisation script
#############################
