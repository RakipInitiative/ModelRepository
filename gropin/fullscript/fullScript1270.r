#############################
# start of Parameter script
#############################
T <- seq(8.008,19.98001998002,length.out=21)
pH <- seq(2.002,11.988011988012,length.out=21)
Hours <- seq(0,479.52047952048,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 1270 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T,pH,Hours)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T,pH,Hours) {
   mumax <-13.616*(-1)+0.203*T+3.0026*pH+8.42249*((17.4+0.0311*Hours+0.001036*T*Hours)/100)-0.00629*T*pH+0.11391*T*((17.4+0.0311*Hours+0.001036*T*Hours)/100)-0.00217*T^2-0.2171*pH^2-93.4381*((17.4+0.0311*Hours+0.001036*T*Hours)/100)^2

return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['pH'],argumentsPar['Hours']))
colnames(responseSurface) <- c(colnames(argumentsPar),'lnmumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 1270 
#############################
argPar1 <- expand.grid(T,pH)
argPar2 <- expand.grid(T,Hours)
argPar3 <- expand.grid(pH,Hours)
z1 <- matrix(unlist(response_surface(argPar1[1],argPar1[2],Hours[10])),nrow=21)
z2 <- matrix(unlist(response_surface(argPar2[1],pH[10],argPar2[2])),nrow=21)
z3 <- matrix(unlist(response_surface(T[10],argPar3[1],argPar3[2])),nrow=21)
par(mfrow = c(1,3))
persp(T,pH,z1,col = 'green',xlab='T',ylab='pH',zlab='ln_mu_max',theta=305,phi=20,shade=0.25,ticktype = 'detailed')
persp(T,Hours,z2,col = 'green',xlab='T',ylab='Hours',zlab='ln_mu_max',theta=305,phi=20,shade=0.25,ticktype = 'detailed')
persp(pH,Hours,z3,col = 'green',xlab='pH',ylab='Hours',zlab='ln_mu_max',theta=305,phi=20,shade=0.25,ticktype = 'detailed')
mtext('Response surface ln_mu_max for
Yersinia enterocolitica in/on Sausages  Italian-fresh pork _80%_
(gropin ID:1270)',outer=T,  cex=1.5, line=-8.5, side=3)
#############################
# End of Visualisation script
#############################
