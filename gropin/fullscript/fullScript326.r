#############################
# start of Parameter script
#############################
T <- seq(5.005,45.3546453546454,length.out=21)
pH <- seq(3.88388,8.9010989010989,length.out=21)
aw <- seq(0.944944,0.999000999000999,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 326 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T,pH,aw)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T,pH,aw) {
   mumax <-4.5*(((T-45.4)*((T-5)^2))/((40.9-5)*((40.9-5)*(T-40.9)-(40.9-45.4)*(40.9+5-2*T))))*(((pH-8.91)*(pH-3.88))/((6.4-3.88)*(pH-6.4)-(6.4-8.91)*(3.88-pH)))*(((aw-1)*((aw-0.944)^2))/((0.996-0.944)*((0.996-0.944)*(aw-0.996)-(0.996-1)*(0.996+0.944-2*aw))))

return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['pH'],argumentsPar['aw']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 326 
#############################
argPar1 <- expand.grid(T,pH)
argPar2 <- expand.grid(T,aw)
argPar3 <- expand.grid(pH,aw)
z1 <- matrix(unlist(response_surface(argPar1[1],argPar1[2],aw[10])),nrow=21)
z2 <- matrix(unlist(response_surface(argPar2[1],pH[10],argPar2[2])),nrow=21)
z3 <- matrix(unlist(response_surface(T[10],argPar3[1],argPar3[2])),nrow=21)
par(mfrow = c(1,3))
persp(T,pH,z1,col = 'green',xlab='T',ylab='pH',zlab='_mu_max',theta=305,phi=20,shade=0.25,ticktype = 'detailed')
persp(T,aw,z2,col = 'green',xlab='T',ylab='aw',zlab='_mu_max',theta=305,phi=20,shade=0.25,ticktype = 'detailed')
persp(pH,aw,z3,col = 'green',xlab='pH',ylab='aw',zlab='_mu_max',theta=305,phi=20,shade=0.25,ticktype = 'detailed')
mtext('Response surface _mu_max for
Escherichia coli O157:H7 in/on Beef meat
(gropin ID:326)',outer=T,  cex=1.5, line=-8.5, side=3)
#############################
# End of Visualisation script
#############################
