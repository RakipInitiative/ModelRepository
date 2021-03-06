#############################
# start of Parameter script
#############################
pH <- seq(1.6983016983017,3.2032,length.out=21)
Sugar <- seq(64.064,67.9320679320679,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 1088 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(pH,Sugar)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(pH,Sugar) {
   mumax <-(-2221.43+85.390*Sugar-0.700*(Sugar^2)-419.81*pH+45.270*(pH^2)+2.45*Sugar*pH)

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['pH'],argumentsPar['Sugar']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 1088 
#############################
titleText <-'Response surface _mu_max for
Zygosaccharomyces rouxii in/on Grape juice _concentrated_
(gropin ID:1088)'
persp(pH,Sugar,matrix(unlist(responseSurface$'mumax'),nrow=21),col = 'green',xlab='pH',ylab='Sugar',zlab='mumax',main=titleText,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
#############################
# End of Visualisation script
#############################
