#############################
# start of Parameter script
#############################
pH <- seq(2.4975024975025,5.5055,length.out=21)
S <- seq(299.7002997003,900.9,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 1050 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(pH,S)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(pH,S) {
   mumax <-0.48*(pH-2.3)*(pH-15.1)/((pH-2.3)*(pH-15.1)-(pH-4)^2)*(1-S/981)

return(mumax=mumax)
} 

# output parameters
mumax <- cbind(argumentsPar,response_surface(argumentsPar['pH'],argumentsPar['S']))
colnames(mumax) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 1050 
#############################
persp(pH,S,matrix(unlist(mumax$mumax),nrow=21),col = 'green',xlab='pH',ylab='S',zlab='mu_max',main='Response surface mu_max for
Zygosaccharomyces rouxii in/on High sugar concentrations
(gropin ID:1050)',theta=305,phi=20,shade=0.25,ticktype = 'detailed')
#############################
# End of Visualisation script
#############################
