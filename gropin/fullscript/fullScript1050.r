#############################
# start of Parameter script
#############################
pH <- seq(2.5025,5.4945054945055,length.out=21)
S <- seq(300.3,899.100899100899,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 1050 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(pH,S)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(pH,S) {
   mumax <-0.48*(pH-2.3)*(pH-15.1)/((pH-2.3)*(pH-15.1)-(pH-4)^2)*(1-S/981)

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['pH'],argumentsPar['S']))
colnames(responseSurface) <- c(colnames(argumentsPar),'lnmumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 1050 
#############################
titleText <-'Response surface ln_mu_max for
Zygosaccharomyces rouxii in/on High sugar concentrations
(gropin ID:1050)'
persp(pH,S,matrix(unlist(responseSurface$'lnmumax'),nrow=21),col = 'green',xlab='pH',ylab='S',zlab='lnmumax',main=titleText,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
#############################
# End of Visualisation script
#############################
