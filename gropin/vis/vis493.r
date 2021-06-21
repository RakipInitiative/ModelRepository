############################# 
# start of Visualisation script Gropin ID 493 
#############################
titleText <-'Response surface _mu_max for
Listeria monocytogenes in/on Cheese _Milk_
(gropin ID:493)'
persp(T,days,matrix(unlist(responseSurface$'mumax'),nrow=21),col = 'green',xlab='T',ylab='days',zlab='mumax',main=titleText,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
#############################
# End of Visualisation script
#############################
