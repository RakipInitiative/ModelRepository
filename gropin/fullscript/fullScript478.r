#############################
# start of Parameter script
#############################
aw <- seq(0.96096,0.992007992007992,length.out=21)
NO2 <- seq(0,314.685314685315,length.out=21)
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
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['aw'],argumentsPar['NO2']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 478 
#############################
persp(aw,NO2,matrix(unlist(responseSurface$'mumax'),nrow=21),col = 'green',xlab='aw',ylab='NO2',zlab='mumax',main='Response surface mumax for
Listeria monocytogenes in/on Meat _Cooked_
(gropin ID:478)',theta=305,phi=20,shade=0.25,ticktype = 'detailed')
#############################
# End of Visualisation script
#############################
