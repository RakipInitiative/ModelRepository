#############################
# start of Parameter script
#############################
T <- seq(3.996003996004,16.016,length.out=21)
CLO <- seq(0,50.05,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 1387 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T,CLO)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T,CLO) {
   mumax <-((48.5183/(T-(-1)))^0.5753)*(((100+CLO)/(100-CLO))^0.5913)

return(mumax=mumax)
} 

# output parameters
mumax <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['CLO']))
colnames(mumax) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 1387 
#############################
persp(T,CLO,matrix(unlist(mumax$mumax),nrow=21),col = 'green',xlab='T',ylab='CLO',zlab='mu_max',main='Response surface mu_max for
Listeria monocytogenes in/on Meats _RTE_
(gropin ID:1387)',theta=305,phi=20,shade=0.25,ticktype = 'detailed')
#############################
# End of Visualisation script
#############################
