#############################
# start of Parameter script
#############################
T <- seq(4.004,24.975024975025,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 316 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T) {
   mumax <-0.026*(T-5.613)

return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 316 
#############################
plot(T,responseSurface$'mumax',xlab='T',
                          ylab='mumax',main='Response surface mumax for
Escherichia coli O157:H7 in/on Melons
(gropin ID:316)')
#############################
# End of Visualisation script
#############################
