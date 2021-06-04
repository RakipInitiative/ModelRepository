#############################
# start of Parameter script
#############################
T <- seq(4.004,15.984015984016,length.out=21)
CLO <- seq(0,49.95004995005,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 1387 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T,CLO)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T,CLO) {
   mumax <-((48.5183/(T-(-1)))^0.5753)*(((100+CLO)/(100-CLO))^0.5913)

return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['CLO']))
colnames(responseSurface) <- c(colnames(argumentsPar),'Sqrmumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 1387 
#############################
persp(T,CLO,matrix(unlist(responseSurface$'Sqrmumax'),nrow=21),col = 'green',xlab='T',ylab='CLO',zlab='Sqrmumax',main='Response surface Sqrmumax for
Listeria monocytogenes in/on Meats _RTE_
(gropin ID:1387)',theta=305,phi=20,shade=0.25,ticktype = 'detailed')
#############################
# End of Visualisation script
#############################
