#############################
# start of Parameter script
#############################
T <- seq(14.014,36.963036963037,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 1307 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T) {
   mumax <-0.0542*(T-12.7)

return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 1307 
#############################
plot(T,responseSurface$'mumax',xlab='T',
                          ylab='mumax',main='Response surface mumax for
Escherichia coli O157:H7 in/on Beef meet _raw_
(gropin ID:1307)')
#############################
# End of Visualisation script
#############################
