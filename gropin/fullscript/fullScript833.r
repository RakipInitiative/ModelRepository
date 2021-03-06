#############################
# start of Parameter script
#############################
T <- seq(10.01,44.955044955045,length.out=21)
aw <- seq(0.88088,0.989010989010989,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 833 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T,aw)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(T,aw) {
   mumax <-26.37*((((T-9.11)^2)*(T-46.45))/((32.11-9.11)*((32.11-9.11)*(T-32.11)-(32.11-46.45)*(32.11+9.11-2*T))))*((((aw-0.893)^2)*(aw-0.993))/((0.985-0.893)*((0.985-0.893)*(aw-0.985)-(0.985-0.993)*(0.985+0.893-2*aw))))

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['aw']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 833 
#############################
titleText <-'Response surface _mu_max for
Byssochlamys fulva in/on Fruit _pasteurised_ juices _apple, orange and peach juice_
(gropin ID:833)'
persp(T,aw,matrix(unlist(responseSurface$'mumax'),nrow=21),col = 'green',xlab='T',ylab='aw',zlab='mumax',main=titleText,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
#############################
# End of Visualisation script
#############################
