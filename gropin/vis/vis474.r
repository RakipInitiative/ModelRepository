############################# 
# start of Visualisation script Gropin ID 474 
#############################
titleText <-'Response surface _mu_max for
Listeria monocytogenes in/on Poultry _Cooked_
(gropin ID:474)'
persp(T,Irradiationdose,matrix(unlist(responseSurface$'mumax'),nrow=21),col = 'green',xlab='T',ylab='Irradiationdose',zlab='mumax',main=titleText,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
#############################
# End of Visualisation script
#############################
