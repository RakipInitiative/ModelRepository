############################# 
# start of Visualisation script Gropin ID 1062 
#############################
titleText <-'Response surface ln_mu_max for
Zygosaccharomyces rouxii in/on Apple juice concentrated
(gropin ID:1062)'
persp(Sugar,pH,matrix(unlist(responseSurface$'lnmumax'),nrow=21),col = 'green',xlab='Sugar',ylab='pH',zlab='lnmumax',main=titleText,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
#############################
# End of Visualisation script
#############################
