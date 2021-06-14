#############################
# start of Parameter script
#############################
T <- seq(22.022,41.958041958042,length.out=21)
pH <- seq(5.005,7.99200799200799,length.out=21)
Oleo <- seq(0,0.799200799200799,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 351 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T,pH,Oleo)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T,pH,Oleo) {
   mumax <-(-5.348+0.162*T+0.319*pH+0.066*Oleo-0.0061*T*pH-0.00091*(T^2)-0.561*(Oleo^2))

return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['pH'],argumentsPar['Oleo']))
colnames(responseSurface) <- c(colnames(argumentsPar),'lnmumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 351 
#############################
argPar1 <- expand.grid(T,pH)
argPar2 <- expand.grid(T,Oleo)
argPar3 <- expand.grid(pH,Oleo)
z1 <- matrix(unlist(response_surface(argPar1[1],argPar1[2],Oleo[10])),nrow=21)
z2 <- matrix(unlist(response_surface(argPar2[1],pH[10],argPar2[2])),nrow=21)
z3 <- matrix(unlist(response_surface(T[10],argPar3[1],argPar3[2])),nrow=21)
par(mfrow = c(1,3))
persp(T,pH,z1,col = 'green',xlab='T',ylab='pH',zlab='ln_mu_max',theta=305,phi=20,shade=0.25,ticktype = 'detailed')
persp(T,Oleo,z2,col = 'green',xlab='T',ylab='Oleo',zlab='ln_mu_max',theta=305,phi=20,shade=0.25,ticktype = 'detailed')
persp(pH,Oleo,z3,col = 'green',xlab='pH',ylab='Oleo',zlab='ln_mu_max',theta=305,phi=20,shade=0.25,ticktype = 'detailed')
mtext('Response surface ln_mu_max for
Salmonella spp. in/on Brain Heart Infusion broth
(gropin ID:351)',outer=T,  cex=1.5, line=-8.5, side=3)
#############################
# End of Visualisation script
#############################
