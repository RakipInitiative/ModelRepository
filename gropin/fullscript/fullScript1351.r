#############################
# start of Parameter script
#############################
aw <- seq(0.8008,0.998001998001998,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 1351 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(aw)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(aw) {
   mumax <-exp(1.561-4.240*sqrt(1-aw)+11.38*(sqrt(1-aw))^2)

return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['aw']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 1351 
#############################
plot(aw,responseSurface$'mumax',xlab='aw',
                          ylab='mumax',main='Response surface mumax for
Aspergillus nomius in/on Basal medium
(gropin ID:1351)')
#############################
# End of Visualisation script
#############################
