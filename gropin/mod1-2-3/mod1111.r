############################# 
# start of Model script Gropin ID 1111 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T,pH)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T,pH) {
   mumax <-sqrt(2.26*((T-68.02)*((T-38.52)^2)/(((57.59-38.52)^(2-1))*((57.59-38.52)*(T-57.59)-(57.59-68.02)*((2-1)*57.59+38.52-2*T))))*((pH-8.91)*((pH-5.27)^1)/(((7.17-5.27)^(0))*((7.17-5.27)*(pH-7.17)-(7.17-8.91)*((0)*7.17+5.27-1*pH)))))

return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['pH']))
colnames(responseSurface) <- c(colnames(argumentsPar),'Sqrmumax')
#############################
# End of Model script
#############################
