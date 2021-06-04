#############################
# start of Parameter script
#############################
T <- seq(0,29.97002997003,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 1199 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T) {
   mumax <-(-4.9969+0.1729*T-0.0011*(T^2))

return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T']))
colnames(responseSurface) <- c(colnames(argumentsPar),'lnmumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 1199 
#############################
plot(T,responseSurface$'lnmumax',xlab='T',
                          ylab='lnmumax',main='Response surface lnmumax for
Staphylococcus aureus in/on Pork _raw_
(gropin ID:1199)')
#############################
# End of Visualisation script
#############################
