#############################
# start of Parameter script
#############################
T <- seq(-1.71828171828172,45.045,length.out=21)
pH <- seq(4.70529470529471,9.61961,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 470 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T,pH)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T,pH) {
   mumax <-(-232.64+((1.4041*(10^5))/(T+273))+((-2.1908*10^7)/((T+273)^2))+(1.1586*(10^2)/pH)+((-4.0952*(10^2))/(pH^2)))

return(mumax=mumax)
} 

# output parameters
mumax <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['pH']))
colnames(mumax) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 470 
#############################
persp(T,pH,matrix(unlist(mumax$mumax),nrow=21),col = 'green',xlab='T',ylab='pH',zlab='mu_max',main='Response surface mu_max for
Listeria monocytogenes in/on Beef _lean_
(gropin ID:470)',theta=305,phi=20,shade=0.25,ticktype = 'detailed')
#############################
# End of Visualisation script
#############################
