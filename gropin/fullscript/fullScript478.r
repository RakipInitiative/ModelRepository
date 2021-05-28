#############################
# start of Parameter script
#############################
aw <- seq(0.959040959040959,0.993993,length.out=21)
NO2 <- seq(0,315.315,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 478 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(aw,NO2)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(aw,NO2) {
   mumax <-1150.5+(-2330.2)*aw+(-0.58142)*((NO2*1000)/(69.01*(1+10^(6.1-3.37))))+0.33406*aw*((NO2*1000)/(69.01*(1+10^(6.1-3.37))))+1182.1*(aw^2)+(-0.019875)*(((NO2*1000)/(69.01*(1+10^(6.1-3.37))))^2)

return(mumax=mumax)
} 

# output parameters
mumax <- cbind(argumentsPar,response_surface(argumentsPar['aw'],argumentsPar['NO2']))
colnames(mumax) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 478 
#############################
persp(aw,NO2,matrix(unlist(mumax$mumax),nrow=21),col = 'green',xlab='aw',ylab='NO2',zlab='mu_max',main='Response surface mu_max for
Listeria monocytogenes in/on Meat _Cooked_
(gropin ID:478)',theta=305,phi=20,shade=0.25,ticktype = 'detailed')
#############################
# End of Visualisation script
#############################
