#############################
# start of Parameter script
#############################
T <- seq(-4.4044,32.4675324675325,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 171 
#############################
 
# constant coefficients for this model
Tmin <- -4.4
Topt <- 23.9
Tmax <- 32.5
 
variables <- data.frame(T)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T) {
   mumax <-((2.4^0.5)*(((T-Tmax)*(T-Tmin)^2)/((Topt-Tmin)*(((Topt-Tmin)*(T-Topt))-((Topt-Tmax)*(Topt+Tmin-2*T)))))^0.5)

return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T']))
colnames(responseSurface) <- c(colnames(argumentsPar),'Sqrmumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 171 
#############################
plot(T,responseSurface$'Sqrmumax',xlab='T',
                          ylab='Sqrmumax',main='Response surface Sqrmumax for
Penicillium expansum in/on Potato Dextrose Agar
(gropin ID:171)')
#############################
# End of Visualisation script
#############################
