############################# 
# start of Visualisation script Gropin ID 421 
#############################
titleText <-'Response surface _mu_max for
Enterobacter agglomerants in/on Tryptic Soy Broth
(gropin ID:421)'
persp(T,pH,matrix(unlist(responseSurface$'mumax'),nrow=21),col = 'green',xlab='T',ylab='pH',zlab='mumax',main=titleText,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
#############################
# End of Visualisation script
#############################
