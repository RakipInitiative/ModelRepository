############################# 
# start of Visualisation script Gropin ID 1111 
#############################
titleText <-'Response surface Sqr_mu_max for
Geobacillus stearothermophilus in/on Canned food
(gropin ID:1111)'
persp(T,pH,matrix(unlist(responseSurface$'Sqrmumax'),nrow=21),col = 'green',xlab='T',ylab='pH',zlab='Sqrmumax',main=titleText,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
#############################
# End of Visualisation script
#############################
