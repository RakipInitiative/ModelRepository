#############################
# start of Parameter script
#############################
T <- seq(5.005,9.99000999000999,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 1287 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(T) {
   mumax <-0.278*T-2.21

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T']))
colnames(responseSurface) <- c(colnames(argumentsPar),'lnmumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 1287 
#############################
titleText <-'Response surface ln_mu_max for
Pseudomonas in/on hake _Merluccius merluccius_ fillets
(gropin ID:1287)'
plot(T,responseSurface$'lnmumax',xlab='T',
                          ylab='lnmumax',main=titleText)
#############################
# End of Visualisation script
#############################
