#############################
# start of Parameter script
#############################
T <- seq(0,9.99000999000999,length.out=21)
CO2 <- seq(30.03,97.9020979020979,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 1299 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T,CO2)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T,CO2) {
   mumax <-6.88*(10^-4)*(T+13.25)*sqrt(452-CO2)

return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['CO2']))
colnames(responseSurface) <- c(colnames(argumentsPar),'Sqrmumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 1299 
#############################
persp(T,CO2,matrix(unlist(responseSurface$'Sqrmumax'),nrow=21),col = 'green',xlab='T',ylab='CO2',zlab='Sqrmumax',main='Response surface Sqrmumax for
Lactococcus spp. in/on Salmon _Atlantic, produced in Australia, MAP_
(gropin ID:1299)',theta=305,phi=20,shade=0.25,ticktype = 'detailed')
#############################
# End of Visualisation script
#############################
