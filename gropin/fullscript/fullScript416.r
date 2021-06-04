#############################
# start of Parameter script
#############################
T <- seq(0,11.988011988012,length.out=21)
aw <- seq(0.913913,0.998001998001998,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 416 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T,aw)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T,aw) {
   mumax <-exp(-919.54+1.6033*((10^5)/(T+273))-2.3784*((10^7)/((T+273)^2))+1317.7*aw-669*(aw^2))

return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['aw']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 416 
#############################
persp(T,aw,matrix(unlist(responseSurface$'mumax'),nrow=21),col = 'green',xlab='T',ylab='aw',zlab='mumax',main='Response surface mumax for
Pseudomonas spp. in/on Ox muscle
(gropin ID:416)',theta=305,phi=20,shade=0.25,ticktype = 'detailed')
#############################
# End of Visualisation script
#############################
