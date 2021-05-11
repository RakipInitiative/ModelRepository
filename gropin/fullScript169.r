#############################
# start of Parameter script
#############################
aw <- seq(0.856,0.982,length.out=21)
notused <- seq(0,1,length.out=21)
awmin <- 0.856
awopt <- 0.981
#############################
# end of Parameter script
#############################
#############################
# start of Model script
#############################
 
variables <- data.frame(aw,notused)
argumentsPar <- expand.grid(variables)
response_surface <- function(aw,notused) {
   mumax <-matrix(unlist((2.86^0.5)*(((aw-1)*(aw-awmin)^2)/((awopt-awmin)*(((awopt-awmin)*(aw-awopt))-((awopt-1)*(awopt+awmin-2*aw)))))^0.5,nrow=21))
return(mumax=mumax)
} 
mumax <- response_surface(argumentsPar['aw'],argumentsPar['notused'])
#############################
# End of Model script
#############################
#############################
# start of Visualisation script
#############################
persp(aw,notused,mumax,col = 'green',xlab='aw',ylab='notused',zlab='mu_max',main='Response surface mu_max for
Penicillium expansum in/on Potato Dextrose Agar
(gropin ID:169)',theta=305,phi=20,shade=0.25,ticktype = 'detailed')
#############################
# End of Visualisation script
#############################
