############################# 
# start of Visualisation script Gropin ID 1129 
#############################
titleText <-'Response surface ln_mu_max for
Staphylococcus aureus in/on RTE pork
(gropin ID:1129)'
persp(T,PL_SDAmix,matrix(unlist(responseSurface$'lnmumax'),nrow=21),col = 'green',xlab='T',ylab='PL_SDAmix',zlab='lnmumax',main=titleText,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
#############################
# End of Visualisation script
#############################
