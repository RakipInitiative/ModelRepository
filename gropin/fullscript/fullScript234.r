#############################
# start of Parameter script
#############################
pH <- seq(3.996003996004,10.01,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 234 
#############################
 
# constant coefficients for this model
mopt <- 1
pHmin <- 4.2
c2 <- 3.2000000000000001E-2
pHmax <- 9.8
pHopt <- 7
 
variables <- data.frame(pH)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(pH) {
   mumax <-mopt*(((pH-pHmin)*(1-exp(c2*(pH-pHmax))))/((pHopt-pHmin)*(1-exp(c2*(pHopt-pHmax)))))^2

return(mumax=mumax)
} 

# output parameters
mumax <- cbind(argumentsPar,response_surface(argumentsPar['pH']))
colnames(mumax) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 234 
#############################
plot(pH,mumax$mumax,
                          xlab='pH',
                          ylab='mu_max')
#############################
# End of Visualisation script
#############################
