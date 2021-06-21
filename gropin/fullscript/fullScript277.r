#############################
# start of Parameter script
#############################
T <- seq(0,14.985014985015,length.out=21)
CO2 <- seq(0,99.9000999000999,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 277 
#############################
 
# constant coefficients for this model
mref <- 0.0556
dco2 <- 0.00815
Ea <- 68.2
 
variables <- data.frame(T,CO2)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(T,CO2) {
   mumax <-(log(mref)-(dco2*CO2)+(Ea/0.00831)*((1/273)-(1/(T+273))))

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['CO2']))
colnames(responseSurface) <- c(colnames(argumentsPar),'lnmumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 277 
#############################
titleText <-'Response surface ln_mu_max for
Brochothrix thermosphacta in/on Fish: Red mullet, gilthead seabream, boque
(gropin ID:277)'
persp(T,CO2,matrix(unlist(responseSurface$'lnmumax'),nrow=21),col = 'green',xlab='T',ylab='CO2',zlab='lnmumax',main=titleText,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
#############################
# End of Visualisation script
#############################
