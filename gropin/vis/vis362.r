############################# 
# start of Visualisation script Gropin ID 362 
#############################
titleText <-'Response surface ln_mu_max for
Lactic acid bacteria in/on Ground meat _pork & beef_
(gropin ID:362)'
persp(T,pH,matrix(unlist(responseSurface$'lnmumax'),nrow=21),col = 'green',xlab='T',ylab='pH',zlab='lnmumax',main=titleText,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
#############################
# End of Visualisation script
#############################
