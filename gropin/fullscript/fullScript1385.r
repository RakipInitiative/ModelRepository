#############################
# start of Parameter script
#############################
T <- seq(0,44.955044955045,length.out=21)
pH <- seq(4.9049,7.99200799200799,length.out=21)
aw <- seq(0.95095,0.998001998001998,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 1385 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T,pH,aw)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T,pH,aw) {
   mumax <-((pH-4.9)*(2*6.5-4.9-pH)/(6.5-4.9)^2)* (aw-0.95)/(1-0.95)*2*((T-0)/(37-0))^2

return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['pH'],argumentsPar['aw']))
colnames(responseSurface) <- c(colnames(argumentsPar),'Sqrmumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 1385 
#############################
argPar1 <- expand.grid(T,pH)
argPar2 <- expand.grid(T,aw)
argPar3 <- expand.grid(pH,aw)
z1 <- matrix(unlist(response_surface(argPar1[1],argPar1[2],aw[10])),nrow=21)
z2 <- matrix(unlist(response_surface(argPar2[1],pH[10],argPar2[2])),nrow=21)
z3 <- matrix(unlist(response_surface(T[10],argPar3[1],argPar3[2])),nrow=21)
par(mfrow = c(1,3))
persp(T,pH,z1,col = 'green',xlab='T',ylab='pH',zlab='Sqr_mu_max',theta=305,phi=20,shade=0.25,ticktype = 'detailed')
persp(T,aw,z2,col = 'green',xlab='T',ylab='aw',zlab='Sqr_mu_max',theta=305,phi=20,shade=0.25,ticktype = 'detailed')
persp(pH,aw,z3,col = 'green',xlab='pH',ylab='aw',zlab='Sqr_mu_max',theta=305,phi=20,shade=0.25,ticktype = 'detailed')
mtext('Response surface Sqr_mu_max for
Bacillus cereus in/on Milk
(gropin ID:1385)',outer=T,  cex=1.5, line=-8.5, side=3)
#############################
# End of Visualisation script
#############################
