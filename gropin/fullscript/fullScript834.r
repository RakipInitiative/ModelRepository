#############################
# start of Parameter script
#############################
T <- seq(10.01,44.955044955045,length.out=21)
aw <- seq(0.88088,0.989010989010989,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 834 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T,aw)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T,aw) {
   mumax <-28.03*((((T-10.58)^2)*(T-43.21))/((32.13-10.58)*((32.13-10.58)*(T-32.13)-(32.13-43.25)*(32.13+10.58-2*T))))*((((aw-0.892)^2)*(aw-0.992))/((0.984-0.892)*((0.984-0.892)*(aw-0.984)-(0.984-0.992)*(0.984+0.892-2*aw))))

return(mumax=mumax)
} 

# output parameters
mumax <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['aw']))
colnames(mumax) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 834 
#############################
persp(T,aw,matrix(unlist(mumax$mumax),nrow=21),col = 'green',xlab='T',ylab='aw',zlab='mu_max',main='Response surface mu_max for
Byssochlamys nivea in/on Fruit _pasteurised_ juices _apple, orange and peach juice_
(gropin ID:834)',theta=305,phi=20,shade=0.25,ticktype = 'detailed')
#############################
# End of Visualisation script
#############################
