#############################
# start of Parameter script
#############################
T <- seq(10.01,44.955044955045,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 1170 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T) {
   mumax <-0.00026*((T+3.04)^2)*(1-exp(0.13*(T-51.86)))

return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 1170 
#############################
plot(T,responseSurface$'mumax',xlab='T',
                          ylab='mumax',main='Response surface mumax for
Salmonella spp. in/on Chicken
(gropin ID:1170)')
#############################
# End of Visualisation script
#############################
