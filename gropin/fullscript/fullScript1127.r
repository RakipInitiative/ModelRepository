#############################
# start of Parameter script
#############################
T <- seq(10.01,29.97002997003,length.out=21)
PL_SDAmix <- seq(0,2.997002997003,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 1127 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T,PL_SDAmix)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T,PL_SDAmix) {
   mumax <-(-0.1831)+0.0172*T-0.0231*PL_SDAmix-0.0046*T*PL_SDAmix+0.0009*(T^2)+0.0126*(PL_SDAmix^2)

return(mumax=mumax)
} 

# output parameters
mumax <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['PL_SDAmix']))
colnames(mumax) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 1127 
#############################
persp(T,PL_SDAmix,matrix(unlist(mumax$mumax),nrow=21),col = 'green',xlab='T',ylab='PL_SDAmix',zlab='mu_max',main='Response surface mu_max for
Salmonella Typhimurium in/on RTE pork
(gropin ID:1127)',theta=305,phi=20,shade=0.25,ticktype = 'detailed')
#############################
# End of Visualisation script
#############################
