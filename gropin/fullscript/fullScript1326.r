#############################
# start of Parameter script
#############################
T <- seq(0,14.985014985015,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 1326 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T) {
   mumax <-log(0.036)-(91.1/0.00831)*((1/(T+273))-(1/273))

return(mumax=mumax)
} 

# output parameters
mumax <- cbind(argumentsPar,response_surface(argumentsPar['T']))
colnames(mumax) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 1326 
#############################
plot(T,mumax$mumax,
                          xlab='T',
                          ylab='mu_max',main='Response surface mu_max for
Pseudomonas spp. in/on Pork _minced_
(gropin ID:1326)')
#############################
# End of Visualisation script
#############################
