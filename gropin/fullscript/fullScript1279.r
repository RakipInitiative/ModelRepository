#############################
# start of Parameter script
#############################
T <- seq(1.001,6.99300699300699,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 1279 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T) {
   mumax <-(-0.41*T+3.02)

return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 1279 
#############################
plot(T,responseSurface$'mumax',xlab='T',
                          ylab='mumax',main='Response surface mumax for
Aerobic Mesophiles in/on hake _Merluccius merluccius_ fillets
(gropin ID:1279)')
#############################
# End of Visualisation script
#############################
