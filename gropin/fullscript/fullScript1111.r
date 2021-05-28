#############################
# start of Parameter script
#############################
T <- seq(34.965034965035,70.07,length.out=21)
pH <- seq(5.15484515484516,9.08908,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 1111 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T,pH)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T,pH) {
   mumax <-sqrt(2.26*((T-68.02)*((T-38.52)^2)/(((57.59-38.52)^(2-1))*((57.59-38.52)*(T-57.59)-(57.59-68.02)*((2-1)*57.59+38.52-2*T))))*((pH-8.91)*((pH-5.27)^1)/(((7.17-5.27)^(0))*((7.17-5.27)*(pH-7.17)-(7.17-8.91)*((0)*7.17+5.27-1*pH)))))

return(mumax=mumax)
} 

# output parameters
mumax <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['pH']))
colnames(mumax) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 1111 
#############################
persp(T,pH,matrix(unlist(mumax$mumax),nrow=21),col = 'green',xlab='T',ylab='pH',zlab='mu_max',main='Response surface mu_max for
Geobacillus stearothermophilus in/on Canned food
(gropin ID:1111)',theta=305,phi=20,shade=0.25,ticktype = 'detailed')
#############################
# End of Visualisation script
#############################
