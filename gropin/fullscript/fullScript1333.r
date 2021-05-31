#############################
# start of Parameter script
#############################
pH <- seq(7.007,36.963036963037,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 1333 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(pH)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(pH) {
   mumax <-0.020*(pH+0.57)

return(mumax=mumax)
} 

# output parameters
mumax <- cbind(argumentsPar,response_surface(argumentsPar['pH']))
colnames(mumax) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 1333 
#############################
plot(pH,mumax$mumax,
                          xlab='pH',
                          ylab='mu_max',main='Response surface mu_max for
Salmonella Enterica in/on Leafy greens
(gropin ID:1333)')
#############################
# End of Visualisation script
#############################
