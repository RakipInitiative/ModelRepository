#############################
# start of Parameter script
#############################
Sugar <- seq(66.066,69.9300699300699,length.out=21)
pH <- seq(2.2977022977023,3.5035,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 1064 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(Sugar,pH)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(Sugar,pH) {
   mumax <-4655.09141-141.06579*Sugar+1.23229*(Sugar^2)-89.94556*pH+71.96275*(pH^2)-5.50379*Sugar*pH

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['Sugar'],argumentsPar['pH']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 1064 
#############################
titleText <-'Response surface _mu_max for
Zygosaccharomyces rouxii in/on Apple juice concentrated
(gropin ID:1064)'
persp(Sugar,pH,matrix(unlist(responseSurface$'mumax'),nrow=21),col = 'green',xlab='Sugar',ylab='pH',zlab='mumax',main=titleText,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
#############################
# End of Visualisation script
#############################
