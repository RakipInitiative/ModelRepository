#############################
# start of Parameter script
#############################
O2 <- seq(0.0999000999000999,21.021,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 1074 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(O2)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(O2) {
   mumax <-0.632*O2/(0.118+O2)

return(mumax=mumax)
} 

# output parameters
mumax <- cbind(argumentsPar,response_surface(argumentsPar['O2']))
colnames(mumax) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 1074 
#############################
plot(O2,mumax$mumax,
                          xlab='O2',
                          ylab='mu_max')
#############################
# End of Visualisation script
#############################
