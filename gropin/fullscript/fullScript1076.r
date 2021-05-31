#############################
# start of Parameter script
#############################
O2 <- seq(0.2002,3.996003996004,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 1076 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(O2)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(O2) {
   mumax <-0.674*(1-(O2/3.26)^2.983)

return(mumax=mumax)
} 

# output parameters
mumax <- cbind(argumentsPar,response_surface(argumentsPar['O2']))
colnames(mumax) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 1076 
#############################
plot(O2,mumax$mumax,
                          xlab='O2',
                          ylab='mu_max',main='Response surface mu_max for
Clostridium sporogenes in/on Food products _in modified atmosphere packaging_
(gropin ID:1076)')
#############################
# End of Visualisation script
#############################
