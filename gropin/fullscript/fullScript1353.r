#############################
# start of Parameter script
#############################
bw <- seq(0.032032,0.446753246753247,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 1353 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(bw)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(bw) {
   mumax <-(2.351-2.531*bw+22.02*bw^2)

return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['bw']))
colnames(responseSurface) <- c(colnames(argumentsPar),'lnmumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 1353 
#############################
plot(bw,responseSurface$'lnmumax',xlab='bw',
                          ylab='lnmumax',main='Response surface lnmumax for
Aspergillus nomius in/on Basal medium
(gropin ID:1353)')
#############################
# End of Visualisation script
#############################
