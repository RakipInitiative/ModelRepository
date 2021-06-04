#############################
# start of Parameter script
#############################
T <- seq(-0.4004,28.1718281718282,length.out=21)
aw <- seq(0.947947,0.995004995004995,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 430 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T,aw)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T,aw) {
   mumax <-0.03346*(T+7.6)*sqrt(aw-0.947)

return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['aw']))
colnames(responseSurface) <- c(colnames(argumentsPar),'Sqrmumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 430 
#############################
persp(T,aw,matrix(unlist(responseSurface$'Sqrmumax'),nrow=21),col = 'green',xlab='T',ylab='aw',zlab='Sqrmumax',main='Response surface Sqrmumax for
Pseudomonas spp. in/on Nutrient broth
(gropin ID:430)',theta=305,phi=20,shade=0.25,ticktype = 'detailed')
#############################
# End of Visualisation script
#############################
