############################# 
# start of Visualisation script Gropin ID 1294 
#############################
titleText <-'Response surface Sqr_mu_max for
Carnobacterium maltaromaticum in/on Salmon _Atlantic, produced in Australia, MAP_
(gropin ID:1294)'
persp(T,CO2,matrix(unlist(responseSurface$'Sqrmumax'),nrow=21),col = 'green',xlab='T',ylab='CO2',zlab='Sqrmumax',main=titleText,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
#############################
# End of Visualisation script
#############################
