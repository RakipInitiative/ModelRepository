#############################
# start of Parameter script
#############################
pH <- seq(3.81381,9.69030969030969,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 1198 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(pH)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(pH) {
   mumax <-3.28*(pH-9.84)*((pH-3.78)^2)/((6.81-3.78)*((6.81-3.78)*(pH-6.81)-(6.81-9.84)*(6.81+3.78-2*pH)))

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['pH']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 1198 
#############################
titleText <-'Response surface _mu_max for
Staphylococcus aureus in/on TSB
(gropin ID:1198)'
plot(pH,responseSurface$'mumax',xlab='pH',
                          ylab='mumax',main=titleText)
#############################
# End of Visualisation script
#############################