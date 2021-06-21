############################# 
# start of Visualisation script Gropin ID 275 
#############################
titleText <-'Response surface ln_mu_max for
Pseudomonas spp. in/on Fish: Red mullet, gilthead seabream, boque
(gropin ID:275)'
persp(T,CO2,matrix(unlist(responseSurface$'lnmumax'),nrow=21),col = 'green',xlab='T',ylab='CO2',zlab='lnmumax',main=titleText,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
#############################
# End of Visualisation script
#############################
