############################# 
# start of Visualisation script Gropin ID 828 
#############################
titleText <-'Response surface _mu_max for
Alicyclobacillus acidoterrestris in/on Fruit drinks
(gropin ID:828)'
persp(pH,T,matrix(unlist(responseSurface$'mumax'),nrow=21),col = 'green',xlab='pH',ylab='T',zlab='mumax',main=titleText,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
#############################
# End of Visualisation script
#############################
