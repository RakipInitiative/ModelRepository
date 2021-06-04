#############################
# start of Parameter script
#############################
Sugar <- seq(66.066,69.9300699300699,length.out=21)
pH <- seq(2.2977022977023,3.5035,length.out=21)
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
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['Sugar'],argumentsPar['pH']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 1063 
#############################
persp(Sugar,pH,matrix(unlist(responseSurface$'mumax'),nrow=21),col = 'green',xlab='Sugar',ylab='pH',zlab='mumax',main='Response surface mumax for
Zygosaccharomyces rouxii in/on Apple juice concentrated
(gropin ID:1063)',theta=305,phi=20,shade=0.25,ticktype = 'detailed')
#############################
# End of Visualisation script
#############################
