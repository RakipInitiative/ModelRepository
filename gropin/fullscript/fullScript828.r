#############################
# start of Parameter script
#############################
pH <- seq(3.03303,5.52447552447553,length.out=21)
T <- seq(25.025,54.9450549450549,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 828 
#############################
 
# constant coefficients for this model
pHmin <- 2.93
pHopt <- 4.22
pHmax <- 5.9
Tmin <- 18.11
Topt <- 48.6
Tmax <- 55.68
mopt <- 1.035
 
variables <- data.frame(pH,T)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(pH,T) {
   mumax <-1.035*((pH-pHmax)*(pH-pHmin)/((pHopt-pHmin)*(pH-pHopt)-(pHopt-pHmax)*(pHmin-pH)))*((T-Tmax)*((T-Tmin)^2)/((Topt-Tmin)*((Topt-Tmin)*(T-Topt)-(Topt-Tmax)*(Topt+Tmin-2*T))))

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['pH'],argumentsPar['T']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 828 
#############################
titleText <-'Response surface _mu_max for
Alicyclobacillus acidoterrestris in/on Fruit drinks
(gropin ID:828)'
persp(pH,T,matrix(unlist(responseSurface$'mumax'),nrow=21),col = 'green',xlab='pH',ylab='T',zlab='mumax',main=titleText,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
#############################
# End of Visualisation script
#############################
