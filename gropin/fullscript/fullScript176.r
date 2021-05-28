#############################
# start of Parameter script
#############################
T <- seq(5.99400599400599,31.031,length.out=21)
pH <- seq(4.5954045954046,9.3093,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 176 
#############################
 
# constant coefficients for this model
a11 <- 22.87
pHsubmin <- 4.14
pHsupmax <- 9.8000000000000007
pHmin <- 4.26
pHmax <- 9.77
 
variables <- data.frame(T,pH)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T,pH) {
   mumax <-(a11*(((pH-pHmin)*(pH-pHmax))/((pH-pHsubmin)*(pH-pHsupmax))))

return(mumax=mumax)
} 

# output parameters
mumax <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['pH']))
colnames(mumax) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 176 
#############################
persp(T,pH,matrix(unlist(mumax$mumax),nrow=21),col = 'green',xlab='T',ylab='pH',zlab='mu_max',main='Response surface mu_max for
Lactobacillus curvatus in/on MRS broth
(gropin ID:176)',theta=305,phi=20,shade=0.25,ticktype = 'detailed')
#############################
# End of Visualisation script
#############################
