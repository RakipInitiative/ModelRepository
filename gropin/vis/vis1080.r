############################# 
# start of Visualisation script Gropin ID 1080 
#############################
titleText <-'Response surface _mu_max for
Acinetobacter calcoaceticus in/on Orange juice _Sterilized_
(gropin ID:1080)'
persp(T,Limonin,matrix(unlist(responseSurface$'mumax'),nrow=21),col = 'green',xlab='T',ylab='Limonin',zlab='mumax',main=titleText,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
#############################
# End of Visualisation script
#############################
