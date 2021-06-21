############################# 
# start of Visualisation script Gropin ID 1389 
#############################
titleText <-'Response surface _mu_max for
Listeria monocytogenes in/on Meats _RTE_
(gropin ID:1389)'
persp(T,CLO,matrix(unlist(responseSurface$'mumax'),nrow=21),col = 'green',xlab='T',ylab='CLO',zlab='mumax',main=titleText,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
#############################
# End of Visualisation script
#############################
