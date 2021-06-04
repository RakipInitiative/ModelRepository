#############################
# start of Parameter script
#############################
T <- seq(5.005,9.99000999000999,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 1291 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T) {
   mumax <-0.225*T-1.09

return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T']))
colnames(responseSurface) <- c(colnames(argumentsPar),'lnmumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 1291 
#############################
plot(T,responseSurface$'lnmumax',xlab='T',
                          ylab='lnmumax',main='Response surface lnmumax for
Lactic acid bacteria in/on hake _Merluccius merluccius_ fillets
(gropin ID:1291)')
#############################
# End of Visualisation script
#############################
