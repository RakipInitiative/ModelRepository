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
# start of Model script Gropin ID 1273 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T,pH,Hours)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T,pH,Hours) {
   mumax <-0.898*(T-36.47)*((T-(-5.42))^2)/((31.61-(-5.42))*((31.61-(-5.42)*(T-31.61)-(31.61-36.47)*(31.61+(-5.42)-2*T)))*(pH-10.24)*((pH-5.12))/(1*((7.27-5.12*(pH-7.27)-(7.27-10.24)*(7.27+5.12-pH)))*((1-((17.4+0.0311*Hours+0.001036*T*Hours)/100)^2)-0.997)*(((1-((17.4+0.0311*Hours+0.001036*T*Hours)/100)^2)-0.924))/(1*((0.997-0.924*((1-((17.4+0.0311*Hours+0.001036*T*Hours)/100)^2)-0.997)-(0.997-0.997)*(0.997+0.924-(1-((17.4+0.0311*Hours+0.001036*T*Hours)/100)^2)))))))

return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['pH'],argumentsPar['Hours']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 1273 
#############################
argPar1 <- expand.grid(T,pH)
argPar2 <- expand.grid(T,Hours)
argPar3 <- expand.grid(pH,Hours)
z1 <- matrix(unlist(response_surface(argPar1[1],argPar1[2],Hours[10])),nrow=21)
z2 <- matrix(unlist(response_surface(argPar2[1],pH[10],argPar2[2])),nrow=21)
z3 <- matrix(unlist(response_surface(T[10],argPar3[1],argPar3[2])),nrow=21)
par(mfrow = c(1,3))
persp(T,pH,z1,col = 'green',xlab='T',ylab='pH',zlab='_mu_max',theta=305,phi=20,shade=0.25,ticktype = 'detailed')
persp(T,Hours,z2,col = 'green',xlab='T',ylab='Hours',zlab='_mu_max',theta=305,phi=20,shade=0.25,ticktype = 'detailed')
persp(pH,Hours,z3,col = 'green',xlab='pH',ylab='Hours',zlab='_mu_max',theta=305,phi=20,shade=0.25,ticktype = 'detailed')
mtext('Response surface _mu_max for
Lactic acid bacteria in/on Sausages  Italian-fresh pork _80%_
(gropin ID:1273)',outer=T,  cex=1.5, line=-8.5, side=3)
#############################
# End of Visualisation script
#############################
