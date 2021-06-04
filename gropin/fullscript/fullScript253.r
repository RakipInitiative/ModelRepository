#############################
# start of Parameter script
#############################
T <- seq(0,19.98001998002,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 253 
#############################
 
# constant coefficients for this model
a <- 0.058
Tmin <- -1.03
 
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
# start of Visualisation script Gropin ID 253 
#############################
plot(T,responseSurface$'mumax',xlab='T',
                          ylab='mumax',main='Response surface mumax for
Listeria monocytogenes in/on Cooked ham
(gropin ID:253)')
#############################
# End of Visualisation script
#############################
