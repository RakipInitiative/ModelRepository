#############################
# start of Parameter script
#############################
T <- seq(6.006,30.969030969031,length.out=21)
pH <- seq(4.6046,9.29070929070929,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 174 
#############################
 
# constant coefficients for this model
b13 <- 0.0001263
Tmin <- -3.27
pHmin <- 4.26
pHmax <- 9.77
 
variables <- data.frame(T,pH)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T,pH) {
   mumax <-((-b13)*((T-Tmin)^2)*(pH-pHmin)*(pH-pHmax))

return(mumax=mumax)
} 

# output parameters
mumax <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['pH']))
colnames(mumax) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 174 
#############################
persp(T,pH,matrix(unlist(mumax$mumax),nrow=21),col = 'green',xlab='T',ylab='pH',zlab='mu_max',main='Response surface mu_max for
Lactobacillus curvatus in/on MRS broth
(gropin ID:174)',theta=305,phi=20,shade=0.25,ticktype = 'detailed')
#############################
# End of Visualisation script
#############################
