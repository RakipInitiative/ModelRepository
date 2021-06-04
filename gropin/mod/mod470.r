############################# 
# start of Model script Gropin ID 470 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T,pH)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T,pH) {
   mumax <-(-232.64+((1.4041*(10^5))/(T+273))+((-2.1908*10^7)/((T+273)^2))+(1.1586*(10^2)/pH)+((-4.0952*(10^2))/(pH^2)))

return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['pH']))
colnames(responseSurface) <- c(colnames(argumentsPar),'lnmumax')
#############################
# End of Model script
#############################
