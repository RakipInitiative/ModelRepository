#############################
# start of Parameter script
#############################
pH <- seq(4.004,11.988011988012,length.out=21)
T <- seq(4.004,44.955044955045,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 235 
#############################
 
# constant coefficients for this model
pHmin <- 3.88
pHopt <- 7.2
pHmax <- 12.17
Tmin <- 3.06
Topt <- 41.1
Tmax <- 45.06
mopt <- 2.635
 
variables <- data.frame(pH,T)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(pH,T) {
   mumax <-2.635*(((pH-pHmin)*(pH-pHmax)/(((pH-pHmin)*(pH-pHmax))-(pH-pHopt)^2)))*((T-Tmax)*(T-Tmin)^2)/((Topt-Tmin)*((Topt-Tmin)*(T-Topt)-(Topt-Tmax)*(Topt+Tmin-2*T)))

return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['pH'],argumentsPar['T']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 235 
#############################
persp(pH,T,matrix(unlist(responseSurface$'mumax'),nrow=21),col = 'green',xlab='pH',ylab='T',zlab='mumax',main='Response surface mumax for
Escherichia coli O157:H7 in/on Nutrient broth
(gropin ID:235)',theta=305,phi=20,shade=0.25,ticktype = 'detailed')
#############################
# End of Visualisation script
#############################
