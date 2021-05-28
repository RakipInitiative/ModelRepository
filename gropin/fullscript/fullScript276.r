#############################
# start of Parameter script
#############################
T <- seq(0,15.015,length.out=21)
CO2 <- seq(0,100.1,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 276 
#############################
 
# constant coefficients for this model
mref <- 0.065
dco2 <- 0.0103
Ea <- 70.3
 
variables <- data.frame(T,CO2)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T,CO2) {
   mumax <-(log(mref)-(dco2*CO2)+(Ea/0.00831)*((1/273)-(1/(T+273))))

return(mumax=mumax)
} 

# output parameters
mumax <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['CO2']))
colnames(mumax) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 276 
#############################
persp(T,CO2,matrix(unlist(mumax$mumax),nrow=21),col = 'green',xlab='T',ylab='CO2',zlab='mu_max',main='Response surface mu_max for
Shewanella putrefaciens in/on Fish: Red mullet, gilthead seabream, boque
(gropin ID:276)',theta=305,phi=20,shade=0.25,ticktype = 'detailed')
#############################
# End of Visualisation script
#############################
