#############################
# start of Parameter script
#############################
O2 <- seq(0.1001,5.99400599400599,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 1075 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(O2)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(O2) {
   mumax <-0.368*(1-(O2/6.61)^0.764)

return(mumax=mumax)
} 

# output parameters
mumax <- cbind(argumentsPar,response_surface(argumentsPar['O2']))
colnames(mumax) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 1075 
#############################
plot(O2,mumax$mumax,
                          xlab='O2',
                          ylab='mu_max',main='Response surface mu_max for
Clostridium perfringens in/on Food products _in modified atmosphere packaging_
(gropin ID:1075)')
#############################
# End of Visualisation script
#############################
