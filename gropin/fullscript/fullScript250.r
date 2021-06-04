#############################
# start of Parameter script
#############################
T <- seq(0,24.975024975025,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 250 
#############################
 
# constant coefficients for this model
a <- 0.019
Tmin <- -10.7
 
variables <- data.frame(T)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T) {
   mumax <-2.302585*(a*(T-Tmin))^2/24

return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 250 
#############################
plot(T,responseSurface$'mumax',xlab='T',
                          ylab='mumax',main='Response surface mumax for
Lactic acid bacteria in/on Cheese salad _pH 4.5_
(gropin ID:250)')
#############################
# End of Visualisation script
#############################
