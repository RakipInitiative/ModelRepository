############################# 
# start of Visualisation script Gropin ID 1331 
#############################
titleText <-'Response surface _mu_max for
Salmonella ser.Newport in/on TSB
(gropin ID:1331)'
persp(pH,aw,matrix(unlist(responseSurface$'mumax'),nrow=21),col = 'green',xlab='pH',ylab='aw',zlab='mumax',main=titleText,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
#############################
# End of Visualisation script
#############################
