#############################
# start of Parameter script
#############################
pH <- seq(1.6983016983017,3.2032,length.out=21)
Sugar <- seq(64.064,67.9320679320679,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 1090 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(pH,Sugar)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(pH,Sugar) {
   mumax <-2771.561-75.864*Sugar+0.573*(Sugar^2)-228.12*pH+27.544*(pH^2)+0.956*Sugar*pH

return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['pH'],argumentsPar['Sugar']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 1090 
#############################
persp(pH,Sugar,matrix(unlist(responseSurface$'mumax'),nrow=21),col = 'green',xlab='pH',ylab='Sugar',zlab='mumax',main='Response surface mumax for
Zygosaccharomyces rouxii in/on Grape juice _concentrated_
(gropin ID:1090)',theta=305,phi=20,shade=0.25,ticktype = 'detailed')
#############################
# End of Visualisation script
#############################
