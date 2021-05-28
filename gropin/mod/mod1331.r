############################# 
# start of Model script Gropin ID 1331 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(pH,aw)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(pH,aw) {
   mumax <-((pH-9.71)*(pH-3.58)/((6.02-3.58)*(pH-6.02)-(6.02-9.71)*(3.58-pH)))*((aw-1)*((aw-1)^2)/((0.994-0.894)*((0.994-0.894)*(aw-0.994)-(0.994-1)*(0.994+0.894-2*aw))))

return(mumax=mumax)
} 

# output parameters
mumax <- cbind(argumentsPar,response_surface(argumentsPar['pH'],argumentsPar['aw']))
colnames(mumax) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
