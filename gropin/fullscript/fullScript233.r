#############################
# start of Parameter script
#############################
pH <- seq(4.004,9.99000999000999,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 233 
#############################
 
# constant coefficients for this model
pHmin <- 4.2
pHmax <- 9.8
b2 <- 4.01
c1 <- 0.032
 
variables <- data.frame(pH)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(pH) {
   mumax <-b2*((pH-pHmin)*(1-exp(c1*(pH-pHmax))))^2

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['pH']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 233 
#############################
titleText <-'Response surface _mu_max for
Listeria monocytogenes in/on Nutrient broth
(gropin ID:233)'
plot(pH,responseSurface$'mumax',xlab='pH',
                          ylab='mumax',main=titleText)
#############################
# End of Visualisation script
#############################
