############################# 
# start of Visualisation script Gropin ID 416 
#############################
titleText <-'Response surface _mu_max for
Pseudomonas spp. in/on Ox muscle
(gropin ID:416)'
persp(T,aw,matrix(unlist(responseSurface$'mumax'),nrow=21),col = 'green',xlab='T',ylab='aw',zlab='mumax',main=titleText,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
#############################
# End of Visualisation script
#############################
