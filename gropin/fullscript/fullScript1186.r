#############################
# start of Parameter script
#############################
T <- seq(10.01,39.96003996004,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 1186 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(T) {
   mumax <-(-0.150*(T-0.173))

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T']))
colnames(responseSurface) <- c(colnames(argumentsPar),'Sqrmumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 1186 
#############################
titleText <-'Response surface Sqr_mu_max for
Staphylococcus aureus in/on Pork, poultry & meat _raw_
(gropin ID:1186)'
plot(T,responseSurface$'Sqrmumax',xlab='T',
                          ylab='Sqrmumax',main=titleText)
#############################
# End of Visualisation script
#############################
