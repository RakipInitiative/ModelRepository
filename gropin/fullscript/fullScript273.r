#############################
# start of Parameter script
#############################
T <- seq(0,19.98001998002,length.out=21)
pH <- seq(5.34534,6.12387612387612,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 273 
#############################
 
# constant coefficients for this model
mref <- 0.02
dm <- 0
Ea <- 99.6
 
variables <- data.frame(T,pH)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T,pH) {
   mumax <-(log(mref)-dm*(5.7-pH)-(Ea/0.00831)*((1/(T+273))-(1/273)))

return(mumax=mumax)
} 

# output parameters
mumax <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['pH']))
colnames(mumax) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 273 
#############################
persp(T,pH,matrix(unlist(mumax$mumax),nrow=21),col = 'green',xlab='T',ylab='pH',zlab='mu_max',main='Response surface mu_max for
Lactic acid bacteria in/on Ground meat _pork & beef_
(gropin ID:273)',theta=305,phi=20,shade=0.25,ticktype = 'detailed')
#############################
# End of Visualisation script
#############################
