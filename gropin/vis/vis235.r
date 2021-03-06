############################# 
# start of Visualisation script Gropin ID 235 
#############################
titleText <-'Response surface _mu_max for
Escherichia coli O157:H7 in/on Nutrient broth
(gropin ID:235)'
persp(pH,T,matrix(unlist(responseSurface$'mumax'),nrow=21),col = 'green',xlab='pH',ylab='T',zlab='mumax',main=titleText,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
#############################
# End of Visualisation script
#############################
