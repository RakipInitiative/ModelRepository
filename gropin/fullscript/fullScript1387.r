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
argumentsPar <- unique.data.frame(expand.grid(variables))
 
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
titleText <-'Response surface Sqr_mu_max for
Listeria monocytogenes in/on Meats _RTE_
(gropin ID:1387)'
persp(T,CLO,matrix(unlist(responseSurface$'Sqrmumax'),nrow=21),col = 'green',xlab='T',ylab='CLO',zlab='Sqrmumax',main=titleText,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
#############################
# End of Visualisation script
#############################
