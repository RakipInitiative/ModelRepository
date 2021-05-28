#############################
# start of Parameter script
#############################
O2 <- seq(4.995004995005,75.075,length.out=21)
CO2 <- seq(0,15.015,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 1274 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(O2,CO2)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(O2,CO2) {
   mumax <-0.395-0.00133*O2-0.00641*CO2

return(mumax=mumax)
} 

# output parameters
mumax <- cbind(argumentsPar,response_surface(argumentsPar['O2'],argumentsPar['CO2']))
colnames(mumax) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 1274 
#############################
persp(O2,CO2,matrix(unlist(mumax$mumax),nrow=21),col = 'green',xlab='O2',ylab='CO2',zlab='mu_max',main='Response surface mu_max for
Pseudomonas fluorescens in/on Lettuce_fresh-cut butterhead_
(gropin ID:1274)',theta=305,phi=20,shade=0.25,ticktype = 'detailed')
#############################
# End of Visualisation script
#############################
