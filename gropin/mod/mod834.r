############################# 
# start of Model script Gropin ID 834 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T,aw)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T,aw) {
   mumax <-28.03*((((T-10.58)^2)*(T-43.21))/((32.13-10.58)*((32.13-10.58)*(T-32.13)-(32.13-43.25)*(32.13+10.58-2*T))))*((((aw-0.892)^2)*(aw-0.992))/((0.984-0.892)*((0.984-0.892)*(aw-0.984)-(0.984-0.992)*(0.984+0.892-2*aw))))

return(mumax=mumax)
} 

# output parameters
mumax <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['aw']))
colnames(mumax) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
