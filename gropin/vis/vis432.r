############################# 
# start of Visualisation script Gropin ID 432 
#############################
titleText <-'Response surface Sqr_mu_max for
Pseudomonas spp. in/on Nutrient broth
(gropin ID:432)'
persp(T,aw,matrix(unlist(responseSurface$'Sqrmumax'),nrow=21),col = 'green',xlab='T',ylab='aw',zlab='Sqrmumax',main=titleText,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
#############################
# End of Visualisation script
#############################
