############################# 
# start of Visualisation script Gropin ID 361 
#############################
titleText <-'Response surface ln_mu_max for
Brochothrix thermosphacta in/on Ground meat _pork & beef_
(gropin ID:361)'
persp(T,pH,matrix(unlist(responseSurface$'lnmumax'),nrow=21),col = 'green',xlab='T',ylab='pH',zlab='lnmumax',main=titleText,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
#############################
# End of Visualisation script
#############################
