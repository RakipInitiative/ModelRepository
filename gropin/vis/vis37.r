############################# 
# start of Visualisation script Gropin ID 37 
#############################
titleText <-'Response surface _mu_max for
Botrytis cinerea in/on Grape berry of "Red Globe" cultivars
(gropin ID:37)'
persp(T,aw,matrix(unlist(responseSurface$'mumax'),nrow=21),col = 'green',xlab='T',ylab='aw',zlab='mumax',main=titleText,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
#############################
# End of Visualisation script
#############################
