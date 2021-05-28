############################# 
# start of Model script Gropin ID 176 
#############################
 
# constant coefficients for this model
a11 <- 22.87
pHsubmin <- 4.14
pHsupmax <- 9.8000000000000007
pHmin <- 4.26
pHmax <- 9.77
 
variables <- data.frame(T,pH)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T,pH) {
   mumax <-(a11*(((pH-pHmin)*(pH-pHmax))/((pH-pHsubmin)*(pH-pHsupmax))))

return(mumax=mumax)
} 

# output parameters
mumax <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['pH']))
colnames(mumax) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
