#############################
# start of Parameter script
#############################
T <- seq(10.01,50.949050949051,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 1162 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T) {
   mumax <-((sqrt(0.047^2)*sqrt((T-10.69)^2)*sqrt((sqrt(1-exp(0.252*(T-52.8))))^2))^(-1))*log(1+(1/0.0137))

return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 1162 
#############################
plot(T,responseSurface$'mumax',xlab='T',
                          ylab='mumax',main='Response surface mumax for
Clostridium perfringens in/on Pork _Uncured_
(gropin ID:1162)')
#############################
# End of Visualisation script
#############################
