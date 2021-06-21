############################# 
# start of Visualisation script Gropin ID 174 
#############################
titleText <-'Response surface _mu_max for
Lactobacillus curvatus in/on MRS broth
(gropin ID:174)'
persp(T,pH,matrix(unlist(responseSurface$'mumax'),nrow=21),col = 'green',xlab='T',ylab='pH',zlab='mumax',main=titleText,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
#############################
# End of Visualisation script
#############################
