#############################
# start of Parameter script
#############################
T <- seq(4.004,36.963036963037,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 1195 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T) {
   mumax <-(-0.451*T+0.006*(T^2)+8.12)

return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T']))
colnames(responseSurface) <- c(colnames(argumentsPar),'lnmumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 1195 
#############################
plot(T,responseSurface$'lnmumax',xlab='T',
                          ylab='lnmumax',main='Response surface lnmumax for
Staphylococcus aureus in/on Eggs _peeled_
(gropin ID:1195)')
#############################
# End of Visualisation script
#############################
