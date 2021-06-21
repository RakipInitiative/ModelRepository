############################# 
# start of Visualisation script Gropin ID 23 
#############################
titleText <-'Response surface _mu_max for
Aeromonas hydrophila in/on modified BHI
(gropin ID:23)'
persp(T,aw,matrix(unlist(responseSurface$'mumax'),nrow=21),col = 'green',xlab='T',ylab='aw',zlab='mumax',main=titleText,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
#############################
# End of Visualisation script
#############################
