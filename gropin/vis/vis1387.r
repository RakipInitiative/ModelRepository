############################# 
# start of Visualisation script Gropin ID 1387 
#############################
titleText <-'Response surface Sqr_mu_max for
Listeria monocytogenes in/on Meats _RTE_
(gropin ID:1387)'
persp(T,CLO,matrix(unlist(responseSurface$'Sqrmumax'),nrow=21),col = 'green',xlab='T',ylab='CLO',zlab='Sqrmumax',main=titleText,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
#############################
# End of Visualisation script
#############################
