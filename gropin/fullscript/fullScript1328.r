#############################
# start of Parameter script
#############################
T <- seq(0,14.985014985015,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 1328 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T) {
   mumax <-log(0.038)-(91.9/0.00831)*((1/(T+273))-(1/273))

return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T']))
colnames(responseSurface) <- c(colnames(argumentsPar),'lnmumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 1328 
#############################
plot(T,responseSurface$'lnmumax',xlab='T',
                          ylab='lnmumax',main='Response surface lnmumax for
Brochothrix thermosphacta in/on Pork _minced_
(gropin ID:1328)')
#############################
# End of Visualisation script
#############################
