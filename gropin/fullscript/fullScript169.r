#############################
# start of Parameter script
#############################
aw <- seq(0.856856,0.981018981018981,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 169 
#############################
 
# constant coefficients for this model
awmin <- 0.856
awopt <- 0.981
 
variables <- data.frame(aw)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(aw) {
   mumax <-(2.86^0.5)*(((aw-1)*(aw-awmin)^2)/((awopt-awmin)*(((awopt-awmin)*(aw-awopt))-((awopt-1)*(awopt+awmin-2*aw)))))^0.5

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['aw']))
colnames(responseSurface) <- c(colnames(argumentsPar),'Sqrmumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 169 
#############################
titleText <-'Response surface Sqr_mu_max for
Penicillium expansum in/on Potato Dextrose Agar
(gropin ID:169)'
plot(aw,responseSurface$'Sqrmumax',xlab='aw',
                          ylab='Sqrmumax',main=titleText)
#############################
# End of Visualisation script
#############################
