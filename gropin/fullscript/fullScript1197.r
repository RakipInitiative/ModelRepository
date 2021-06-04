#############################
# start of Parameter script
#############################
T <- seq(4.004,36.963036963037,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 1197 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T) {
   mumax <-(-0.271*T+0.004*(T^2)+6.44)

return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T']))
colnames(responseSurface) <- c(colnames(argumentsPar),'lnmumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 1197 
#############################
plot(T,responseSurface$'lnmumax',xlab='T',
                          ylab='lnmumax',main='Response surface lnmumax for
Staphylococcus aureus in/on Whole egg liquid
(gropin ID:1197)')
#############################
# End of Visualisation script
#############################
