############################# 
# start of Visualisation script Gropin ID 478 
#############################
titleText <-'Response surface _mu_max for
Listeria monocytogenes in/on Meat _Cooked_
(gropin ID:478)'
persp(aw,NO2,matrix(unlist(responseSurface$'mumax'),nrow=21),col = 'green',xlab='aw',ylab='NO2',zlab='mumax',main=titleText,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
#############################
# End of Visualisation script
#############################
