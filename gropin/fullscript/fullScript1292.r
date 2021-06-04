#############################
# start of Parameter script
#############################
T <- seq(1.001,6.99300699300699,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 1292 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T) {
   mumax <-(-0.77*T+5.27)

return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 1292 
#############################
plot(T,responseSurface$'mumax',xlab='T',
                          ylab='mumax',main='Response surface mumax for
Lactic acid bacteria in/on hake _Merluccius merluccius_ fillets
(gropin ID:1292)')
#############################
# End of Visualisation script
#############################
