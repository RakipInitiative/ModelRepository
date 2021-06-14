############################# 
# start of Model script Gropin ID 1269 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T,pH,Hours)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T,pH,Hours) {
   mumax <-14.02*(-1)+0.2278*T+2.784*pH+7.437*((17.4+0.0311*Hours+0.001036*T*Hours)/100)+0.00696*T*pH-0.06091*T*((17.4+0.0311*Hours+0.001036*T*Hours)/100)+0.3333*pH*((17.4+0.0311*Hours+0.001036*T*Hours)/100)-0.00391*T^2-0.2101*pH^2-52.05*((17.4+0.0311*Hours+0.001036*T*Hours)/100)^2

return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['pH'],argumentsPar['Hours']))
colnames(responseSurface) <- c(colnames(argumentsPar),'lnmumax')
#############################
# End of Model script
#############################
