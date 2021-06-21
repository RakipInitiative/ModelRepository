############################# 
# start of Visualisation script Gropin ID 1318 
#############################
titleText <-'Response surface _mu_max for
Clostridium perfringens in/on Meat _bulk_
(gropin ID:1318)'
persp(T,NaCl,matrix(unlist(responseSurface$'mumax'),nrow=21),col = 'green',xlab='T',ylab='NaCl',zlab='mumax',main=titleText,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
#############################
# End of Visualisation script
#############################
