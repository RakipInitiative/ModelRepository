#############################
# start of Parameter script
#############################
pH <- seq(6.1061,6.79320679320679,length.out=21)
NO2 <- seq(0,314.685314685315,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 479 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(pH,NO2)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(pH,NO2) {
   mumax <-8.0091+(-0.87877)*pH+0.42624*((NO2*1000)/(69.01*(1+10^(pH-3.37))))+0.18149*pH*((NO2*1000)/(69.01*(1+10^(pH-3.37))))+(-0.013099)*(pH^2)+(-0.20018)*(((NO2*1000)/(69.01*(1+10^(pH-3.37))))^2)

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['pH'],argumentsPar['NO2']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 479 
#############################
titleText <-'Response surface _mu_max for
Listeria monocytogenes in/on Meat _Cooked_
(gropin ID:479)'
persp(pH,NO2,matrix(unlist(responseSurface$'mumax'),nrow=21),col = 'green',xlab='pH',ylab='NO2',zlab='mumax',main=titleText,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
#############################
# End of Visualisation script
#############################