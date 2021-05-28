#############################
# start of Parameter script
#############################
Sugar <- seq(65.9340659340659,70.07,length.out=21)
pH <- seq(2.3023,3.4965034965035,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 1063 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(Sugar,pH)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(Sugar,pH) {
   mumax <-4993.00351-147.44162*Sugar+1.25367*(Sugar^2)-161.13055*pH+71.38168*(pH^2)-4.43598*Sugar*pH

return(mumax=mumax)
} 

# output parameters
mumax <- cbind(argumentsPar,response_surface(argumentsPar['Sugar'],argumentsPar['pH']))
colnames(mumax) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 1063 
#############################
persp(Sugar,pH,matrix(unlist(mumax$mumax),nrow=21),col = 'green',xlab='Sugar',ylab='pH',zlab='mu_max',main='Response surface mu_max for
Zygosaccharomyces rouxii in/on Apple juice concentrated
(gropin ID:1063)',theta=305,phi=20,shade=0.25,ticktype = 'detailed')
#############################
# End of Visualisation script
#############################
