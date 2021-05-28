#############################
# start of Parameter script
#############################
pH <- seq(6.99300699300699,35.035,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 1334 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(pH)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(pH) {
   mumax <-0.023*(pH-0.60)

return(mumax=mumax)
} 

# output parameters
mumax <- cbind(argumentsPar,response_surface(argumentsPar['pH']))
colnames(mumax) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 1334 
#############################
plot(pH,mumax$mumax,
                          xlab='pH',
                          ylab='mu_max')
#############################
# End of Visualisation script
#############################
