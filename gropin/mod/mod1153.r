############################# 
# start of Model script Gropin ID 1153 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T) {
   mumax <-0.633*(T-44.3)*((T-11.5)^2)/(((36.4-11.5)*(T-36.4)-(36.4-44.3)*(36.4+11.5-2*T))*(36.4-11.5))

return(mumax=mumax)
} 

# output parameters
mumax <- cbind(argumentsPar,response_surface(argumentsPar['T']))
colnames(mumax) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
