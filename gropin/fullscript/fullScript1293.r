#############################
# start of Parameter script
#############################
T <- seq(0,15.015,length.out=21)
CO2 <- seq(0,100.1,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 1293 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T,CO2)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T,CO2) {
   mumax <-0.082+0.032*T-3.6*(10^-4)-1.1*(10^-4)*T*CO2

return(mumax=mumax)
} 

# output parameters
mumax <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['CO2']))
colnames(mumax) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 1293 
#############################
persp(T,CO2,matrix(unlist(mumax$mumax),nrow=21),col = 'green',xlab='T',ylab='CO2',zlab='mu_max',main='Response surface mu_max for
Photobacterium phosphoreum in/on Cod _Gadus morhua_ fillets _in MAP_
(gropin ID:1293)',theta=305,phi=20,shade=0.25,ticktype = 'detailed')
#############################
# End of Visualisation script
#############################
