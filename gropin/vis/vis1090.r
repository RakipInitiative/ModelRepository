############################# 
# start of Visualisation script Gropin ID 1090 
#############################
titleText <-'Response surface _mu_max for
Zygosaccharomyces rouxii in/on Grape juice _concentrated_
(gropin ID:1090)'
persp(pH,Sugar,matrix(unlist(responseSurface$'mumax'),nrow=21),col = 'green',xlab='pH',ylab='Sugar',zlab='mumax',main=titleText,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
#############################
# End of Visualisation script
#############################
