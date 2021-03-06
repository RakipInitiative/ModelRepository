############################# 
# start of Visualisation script Gropin ID 1275 
#############################
titleText <-'Response surface ln_mu_max for
Pseudomonas fluorescens in/on Lettuce_fresh-cut butterhead_
(gropin ID:1275)'
persp(O2,CO2,matrix(unlist(responseSurface$'lnmumax'),nrow=21),col = 'green',xlab='O2',ylab='CO2',zlab='lnmumax',main=titleText,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
#############################
# End of Visualisation script
#############################
