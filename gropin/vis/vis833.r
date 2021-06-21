############################# 
# start of Visualisation script Gropin ID 833 
#############################
titleText <-'Response surface _mu_max for
Byssochlamys fulva in/on Fruit _pasteurised_ juices _apple, orange and peach juice_
(gropin ID:833)'
persp(T,aw,matrix(unlist(responseSurface$'mumax'),nrow=21),col = 'green',xlab='T',ylab='aw',zlab='mumax',main=titleText,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
#############################
# End of Visualisation script
#############################
