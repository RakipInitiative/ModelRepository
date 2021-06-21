#############################
# start of Parameter script
#############################
T <- seq(-0.4004,39.1608391608392,length.out=21)
aw <- seq(0.947947,0.995004995004995,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 432 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T,aw)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(T,aw) {
   mumax <-0.1709*(T+6.1)*(1-exp(0.1723*(T-41.2)))*sqrt(aw-0.947)

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['aw']))
colnames(responseSurface) <- c(colnames(argumentsPar),'Sqrmumax')
#############################
# End of Model script
#############################
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
