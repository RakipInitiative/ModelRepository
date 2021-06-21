############################# 
# start of Visualisation script Gropin ID 486 
#############################
titleText <-'Response surface _mu_max for
Listeria monocytogenes in/on Pasta salad
(gropin ID:486)'
persp(T,pH,matrix(unlist(responseSurface$'mumax'),nrow=21),col = 'green',xlab='T',ylab='pH',zlab='mumax',main=titleText,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
#############################
# End of Visualisation script
#############################
