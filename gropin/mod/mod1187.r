############################# 
# start of Model script Gropin ID 1187 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T,aw)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T,aw) {
   mumax <-(2.053+(-6.484)* sqrt(1-aw)+1.210*( sqrt(1-aw)* sqrt(1-aw))+0.248*T+0.0015*T*T-0.917* sqrt(1-aw)*T)

return(mumax=mumax)
} 

# output parameters
mumax <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['aw']))
colnames(mumax) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
