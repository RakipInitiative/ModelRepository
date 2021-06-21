############################# 
# start of Visualisation script Gropin ID 27 
#############################
titleText <-'Response surface Sqr_mu_max for
Aeromonas hydrophila in/on modified BHI
(gropin ID:27)'
persp(T,aw,matrix(unlist(responseSurface$'Sqrmumax'),nrow=21),col = 'green',xlab='T',ylab='aw',zlab='Sqrmumax',main=titleText,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
#############################
# End of Visualisation script
#############################
