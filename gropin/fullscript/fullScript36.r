#############################
# start of Parameter script
#############################
aw <- seq(0.895895,0.981018981018981,length.out=21)
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
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['aw']))
colnames(responseSurface) <- c(colnames(argumentsPar),'Sqrmumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 36 
#############################
plot(aw,responseSurface$'Sqrmumax',xlab='aw',
                          ylab='Sqrmumax',main='Response surface Sqrmumax for
Botrytis cinerea in/on Potato Dextrose Agar
(gropin ID:36)')
#############################
# End of Visualisation script
#############################
