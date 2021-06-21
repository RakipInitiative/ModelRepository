############################# 
# start of Visualisation script Gropin ID 1063 
#############################
titleText <-'Response surface _mu_max for
Zygosaccharomyces rouxii in/on Apple juice concentrated
(gropin ID:1063)'
persp(Sugar,pH,matrix(unlist(responseSurface$'mumax'),nrow=21),col = 'green',xlab='Sugar',ylab='pH',zlab='mumax',main=titleText,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
#############################
# End of Visualisation script
#############################
