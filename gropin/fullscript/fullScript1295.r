#############################
# start of Parameter script
#############################
T <- seq(10.01,44.955044955045,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 1295 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T) {
   mumax <-0.00136*((T-2.62)^2)*(1-exp(0.436*(T-45.8)))

return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 1295 
#############################
plot(T,responseSurface$'mumax',xlab='T',
                          ylab='mumax',main='Response surface mumax for
Salmonella spp. in/on Egg products _liquid_
(gropin ID:1295)')
#############################
# End of Visualisation script
#############################
