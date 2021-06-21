#############################
# start of Parameter script
#############################
T <- seq(0,14.985014985015,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 1326 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(T) {
   mumax <-log(0.036)-(91.1/0.00831)*((1/(T+273))-(1/273))

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T']))
colnames(responseSurface) <- c(colnames(argumentsPar),'lnmumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 1326 
#############################
titleText <-'Response surface ln_mu_max for
Pseudomonas spp. in/on Pork _minced_
(gropin ID:1326)'
plot(T,responseSurface$'lnmumax',xlab='T',
                          ylab='lnmumax',main=titleText)
#############################
# End of Visualisation script
#############################
