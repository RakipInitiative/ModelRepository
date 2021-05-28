#############################
# start of Parameter script
#############################
T <- seq(9.99000999000999,30.03,length.out=21)
PL_SDAmix <- seq(0,3.003,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 1129 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T,PL_SDAmix)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T,PL_SDAmix) {
   mumax <-0.1312-0.0322*T+0.0972*PL_SDAmix-0.0080*T*PL_SDAmix+0.0023*(T^2)-0.0076*(PL_SDAmix^2)

return(mumax=mumax)
} 

# output parameters
mumax <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['PL_SDAmix']))
colnames(mumax) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 1129 
#############################
persp(T,PL_SDAmix,matrix(unlist(mumax$mumax),nrow=21),col = 'green',xlab='T',ylab='PL_SDAmix',zlab='mu_max',main='Response surface mu_max for
Staphylococcus aureus in/on RTE pork
(gropin ID:1129)',theta=305,phi=20,shade=0.25,ticktype = 'detailed')
#############################
# End of Visualisation script
#############################
