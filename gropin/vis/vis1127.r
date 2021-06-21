############################# 
# start of Visualisation script Gropin ID 1127 
#############################
titleText <-'Response surface ln_mu_max for
Salmonella Typhimurium in/on RTE pork
(gropin ID:1127)'
persp(T,PL_SDAmix,matrix(unlist(responseSurface$'lnmumax'),nrow=21),col = 'green',xlab='T',ylab='PL_SDAmix',zlab='lnmumax',main=titleText,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
#############################
# End of Visualisation script
#############################
