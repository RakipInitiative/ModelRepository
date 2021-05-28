#############################
# start of Parameter script
#############################
T <- seq(0,20.02,length.out=21)
pH <- seq(5.33466533466533,6.13613,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 272 
#############################
 
# constant coefficients for this model
mref <- 0.045
dm <- 0.583
Ea <- 69.5
 
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
# start of Visualisation script Gropin ID 272 
#############################
persp(T,pH,matrix(unlist(mumax$mumax),nrow=21),col = 'green',xlab='T',ylab='pH',zlab='mu_max',main='Response surface mu_max for
Brochothrix thermosphacta in/on Ground meat _pork & beef_
(gropin ID:272)',theta=305,phi=20,shade=0.25,ticktype = 'detailed')
#############################
# End of Visualisation script
#############################
