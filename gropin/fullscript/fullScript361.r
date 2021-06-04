#############################
# start of Parameter script
#############################
T <- seq(0,19.98001998002,length.out=21)
pH <- seq(5.34534,6.12387612387612,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 361 
#############################
 
# constant coefficients for this model
lagref <- 20.7
dlag <- 1.73
Elag <- 67
 
variables <- data.frame(T,pH)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T,pH) {
   mumax <-(log(lagref)-dlag*(5.7-pH)-(Elag/0.00831)*((1/(T+273))-(1/273)))

return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['pH']))
colnames(responseSurface) <- c(colnames(argumentsPar),'lnmumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 361 
#############################
persp(T,pH,matrix(unlist(responseSurface$'lnmumax'),nrow=21),col = 'green',xlab='T',ylab='pH',zlab='lnmumax',main='Response surface lnmumax for
Brochothrix thermosphacta in/on Ground meat _pork & beef_
(gropin ID:361)',theta=305,phi=20,shade=0.25,ticktype = 'detailed')
#############################
# End of Visualisation script
#############################
