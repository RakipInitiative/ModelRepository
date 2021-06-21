#############################
# start of Parameter script
#############################
T <- seq(10.01,29.97002997003,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 1105 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(T) {
   mumax <-8.760-0.211*T

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T']))
colnames(responseSurface) <- c(colnames(argumentsPar),'lnmumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 1105 
#############################
titleText <-'Response surface ln_mu_max for
Neosartorya fischeri in/on Apple juice _solidified_
(gropin ID:1105)'
plot(T,responseSurface$'lnmumax',xlab='T',
                          ylab='lnmumax',main=titleText)
#############################
# End of Visualisation script
#############################
