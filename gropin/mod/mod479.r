############################# 
# start of Model script Gropin ID 479 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(pH,NO2)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(pH,NO2) {
   mumax <-8.0091+(-0.87877)*pH+0.42624*((NO2*1000)/(69.01*(1+10^(pH-3.37))))+0.18149*pH*((NO2*1000)/(69.01*(1+10^(pH-3.37))))+(-0.013099)*(pH^2)+(-0.20018)*(((NO2*1000)/(69.01*(1+10^(pH-3.37))))^2)

return(mumax=mumax)
} 

# output parameters
mumax <- cbind(argumentsPar,response_surface(argumentsPar['pH'],argumentsPar['NO2']))
colnames(mumax) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
