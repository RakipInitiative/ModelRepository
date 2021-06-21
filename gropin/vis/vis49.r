############################# 
# start of Visualisation script Gropin ID 49 
#############################
titleText <-'Response surface ln_mu_max for
Escherichia coli O157:H7 in/on Brain Heart Infusion agar
(gropin ID:49)'
persp(T,pH,matrix(unlist(responseSurface$'lnmumax'),nrow=21),col = 'green',xlab='T',ylab='pH',zlab='lnmumax',main=titleText,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
#############################
# End of Visualisation script
#############################
