#############################
# start of Parameter script
#############################
T <- seq(-0.4004,28.1718281718282,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 429 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(T) {
   mumax <-0.03346*(T+7.7)

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T']))
colnames(responseSurface) <- c(colnames(argumentsPar),'Sqrmumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 429 
#############################
titleText <-'Response surface Sqr_mu_max for
Pseudomonas spp. in/on Nutrient broth
(gropin ID:429)'
plot(T,responseSurface$'Sqrmumax',xlab='T',
                          ylab='Sqrmumax',main=titleText)
#############################
# End of Visualisation script
#############################
