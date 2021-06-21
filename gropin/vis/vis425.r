############################# 
# start of Visualisation script Gropin ID 425 
#############################
titleText <-'Response surface _mu_max for
Brochothrix thermosphacta in/on Tryptic Soy Broth
(gropin ID:425)'
persp(T,pH,matrix(unlist(responseSurface$'mumax'),nrow=21),col = 'green',xlab='T',ylab='pH',zlab='mumax',main=titleText,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
#############################
# End of Visualisation script
#############################
