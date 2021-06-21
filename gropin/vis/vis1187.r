############################# 
# start of Visualisation script Gropin ID 1187 
#############################
titleText <-'Response surface _mu_max for
Botrytis cinerea in/on PDA
(gropin ID:1187)'
persp(T,aw,matrix(unlist(responseSurface$'mumax'),nrow=21),col = 'green',xlab='T',ylab='aw',zlab='mumax',main=titleText,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
#############################
# End of Visualisation script
#############################
