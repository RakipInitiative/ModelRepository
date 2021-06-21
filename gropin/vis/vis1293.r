############################# 
# start of Visualisation script Gropin ID 1293 
#############################
titleText <-'Response surface Sqr_mu_max for
Photobacterium phosphoreum in/on Cod _Gadus morhua_ fillets _in MAP_
(gropin ID:1293)'
persp(T,CO2,matrix(unlist(responseSurface$'Sqrmumax'),nrow=21),col = 'green',xlab='T',ylab='CO2',zlab='Sqrmumax',main=titleText,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
#############################
# End of Visualisation script
#############################
