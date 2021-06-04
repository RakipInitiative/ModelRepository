#############################
# start of Parameter script
#############################
bw <- seq(0.032032,0.446753246753247,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 1362 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(bw)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(bw) {
   mumax <-(-0.3915+5.571*bw-25.16*bw^2)

return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['bw']))
colnames(responseSurface) <- c(colnames(argumentsPar),'lnmumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 1362 
#############################
plot(bw,responseSurface$'lnmumax',xlab='bw',
                          ylab='lnmumax',main='Response surface lnmumax for
Aspergillus oryzae in/on Basal medium
(gropin ID:1362)')
#############################
# End of Visualisation script
#############################
