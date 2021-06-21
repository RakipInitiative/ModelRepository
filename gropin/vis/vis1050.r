############################# 
# start of Visualisation script Gropin ID 1050 
#############################
titleText <-'Response surface ln_mu_max for
Zygosaccharomyces rouxii in/on High sugar concentrations
(gropin ID:1050)'
persp(pH,S,matrix(unlist(responseSurface$'lnmumax'),nrow=21),col = 'green',xlab='pH',ylab='S',zlab='lnmumax',main=titleText,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
#############################
# End of Visualisation script
#############################
