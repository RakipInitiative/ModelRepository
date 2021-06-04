#############################
# start of Parameter script
#############################
O2 <- seq(0.1001,20.979020979021,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 1074 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(O2)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(O2) {
   mumax <-0.632*O2/(0.118+O2)

return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['O2']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 1074 
#############################
plot(O2,responseSurface$'mumax',xlab='O2',
                          ylab='mumax',main='Response surface mumax for
Pseudomonas fluorescens in/on Food products _in modified atmosphere packaging_
(gropin ID:1074)')
#############################
# End of Visualisation script
#############################
