############################# 
# start of Visualisation script Gropin ID 479 
#############################
titleText <-'Response surface _mu_max for
Listeria monocytogenes in/on Meat _Cooked_
(gropin ID:479)'
persp(pH,NO2,matrix(unlist(responseSurface$'mumax'),nrow=21),col = 'green',xlab='pH',ylab='NO2',zlab='mumax',main=titleText,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
#############################
# End of Visualisation script
#############################
