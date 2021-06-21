#############################
# start of Parameter script
#############################
T <- seq(4.995004995005,25.025,length.out=21)
aw <- seq(0.89089,0.994005994005994,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 1189 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T,aw)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(T,aw) {
   mumax <-(-0.417+(30.548)* sqrt(1-aw)-92.980*( sqrt(1-aw)* sqrt(1-aw))+0.242*T+0.0018*T*T-0.849* sqrt(1-aw)*T)

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['aw']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 1189 
#############################
titleText <-'Response surface _mu_max for
Botrytis cinerea in/on PDA
(gropin ID:1189)'
persp(T,aw,matrix(unlist(responseSurface$'mumax'),nrow=21),col = 'green',xlab='T',ylab='aw',zlab='mumax',main=titleText,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
#############################
# End of Visualisation script
#############################
