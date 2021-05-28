#############################
# start of Parameter script
#############################
aw <- seq(0.894105894105894,0.982982,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 36 
#############################
 
# constant coefficients for this model
awmin <- 0.895
awopt <- 0.988
 
variables <- data.frame(aw)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(aw) {
   mumax <-(9.97^0.5)*(((aw-1)*(aw-awmin)^2)/((awopt-awmin)*(((awopt-awmin)*(aw-awopt))-((awopt-1)*(awopt+awmin-2*aw)))))^0.5

return(mumax=mumax)
} 

# output parameters
mumax <- cbind(argumentsPar,response_surface(argumentsPar['aw']))
colnames(mumax) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 36 
#############################
plot(aw,mumax$mumax,
                          xlab='aw',
                          ylab='mu_max')
#############################
# End of Visualisation script
#############################
