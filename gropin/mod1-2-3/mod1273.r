############################# 
# start of Model script Gropin ID 1273 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T,pH,Hours)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T,pH,Hours) {
   mumax <-0.898*(T-36.47)*((T-(-5.42))^2)/((31.61-(-5.42))*((31.61-(-5.42)*(T-31.61)-(31.61-36.47)*(31.61+(-5.42)-2*T)))*(pH-10.24)*((pH-5.12))/(1*((7.27-5.12*(pH-7.27)-(7.27-10.24)*(7.27+5.12-pH)))*((1-((17.4+0.0311*Hours+0.001036*T*Hours)/100)^2)-0.997)*(((1-((17.4+0.0311*Hours+0.001036*T*Hours)/100)^2)-0.924))/(1*((0.997-0.924*((1-((17.4+0.0311*Hours+0.001036*T*Hours)/100)^2)-0.997)-(0.997-0.997)*(0.997+0.924-(1-((17.4+0.0311*Hours+0.001036*T*Hours)/100)^2)))))))

return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['pH'],argumentsPar['Hours']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
