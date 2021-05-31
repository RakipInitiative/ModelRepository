#############################
# start of Parameter script
#############################
T <- seq(10.01,39.96003996004,length.out=21)
pH <- seq(5.005,6.99300699300699,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 49 
#############################
 
# constant coefficients for this model
a0 <- -10.05
a1 <- 0.34
a2 <- 1.3
a3 <- -0.0048
a4 <- 0.00072
a5 <- -0.1
 
variables <- data.frame(T,pH)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T,pH) {
   mumax <-(a0+(a1*T)+(a2*pH)+(a3*(T^2))+(a4*T*pH)+(a5*(pH^2)))

return(mumax=mumax)
} 

# output parameters
mumax <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['pH']))
colnames(mumax) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 49 
#############################
persp(T,pH,matrix(unlist(mumax$mumax),nrow=21),col = 'green',xlab='T',ylab='pH',zlab='mu_max',main='Response surface mu_max for
Escherichia coli O157:H7 in/on Brain Heart Infusion agar
(gropin ID:49)',theta=305,phi=20,shade=0.25,ticktype = 'detailed')
#############################
# End of Visualisation script
#############################
