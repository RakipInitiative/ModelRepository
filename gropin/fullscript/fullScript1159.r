#############################
# start of Parameter script
#############################
T <- seq(10.01,50.949050949051,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 1159 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T) {
   mumax <-(0.047^2)*((T-10.69)^2)*(sqrt(1-exp(0.252*(T-52.8))))^2

return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T']))
colnames(responseSurface) <- c(colnames(argumentsPar),'Sqrmumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 1159 
#############################
plot(T,responseSurface$'Sqrmumax',xlab='T',
                          ylab='Sqrmumax',main='Response surface Sqrmumax for
Clostridium perfringens in/on Pork _Uncured_
(gropin ID:1159)')
#############################
# End of Visualisation script
#############################
