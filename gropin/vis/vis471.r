############################# 
# start of Visualisation script Gropin ID 471 
#############################
titleText <-'Response surface ln_mu_max for
Listeria monocytogenes in/on Poultry _Raw_
(gropin ID:471)'
persp(T,Irradiationdose,matrix(unlist(responseSurface$'lnmumax'),nrow=21),col = 'green',xlab='T',ylab='Irradiationdose',zlab='lnmumax',main=titleText,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
#############################
# End of Visualisation script
#############################
