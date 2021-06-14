############################# 
# start of Model script Gropin ID 436 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T,pH,NaCl)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T,pH,NaCl) {
   mumax <-152.04-(93751.2/(T+273))-1.64*NaCl+5.49*pH+(12593197/((T+273)^2))-0.06*(NaCl^2)-0.39*(pH^2)+(204.27*NaCl/(T+273))+0.13*pH*NaCl

return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['pH'],argumentsPar['NaCl']))
colnames(responseSurface) <- c(colnames(argumentsPar),'lnmumax')
#############################
# End of Model script
#############################
