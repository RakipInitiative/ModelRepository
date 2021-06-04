#############################
# start of Parameter script
#############################
T <- seq(6.006,30.969030969031,length.out=21)
pH <- seq(4.6046,9.29070929070929,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 175 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T,pH)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T,pH) {
   mumax <-1/(0.00000418*((pH-4.26)^2)*((pH-9.77)^2)*((T+3.27)^2))

return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['pH']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 175 
#############################
persp(T,pH,matrix(unlist(responseSurface$'mumax'),nrow=21),col = 'green',xlab='T',ylab='pH',zlab='mumax',main='Response surface mumax for
Lactobacillus curvatus in/on MRS broth
(gropin ID:175)',theta=305,phi=20,shade=0.25,ticktype = 'detailed')
#############################
# End of Visualisation script
#############################
