############################# 
# start of Visualisation script Gropin ID 470 
#############################
titleText <-'Response surface ln_mu_max for
Listeria monocytogenes in/on Beef _lean_
(gropin ID:470)'
persp(T,pH,matrix(unlist(responseSurface$'lnmumax'),nrow=21),col = 'green',xlab='T',ylab='pH',zlab='lnmumax',main=titleText,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
#############################
# End of Visualisation script
#############################
