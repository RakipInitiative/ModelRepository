############################# 
# start of Model script Gropin ID 1040 
#############################
# variables of this model
T <- seq(T_start, T_end ,length.out=21)
pH <- seq(pH_start, pH_end ,length.out=21)
aw <- seq(aw_start, aw_end ,length.out=21)
 
# constant coefficients for this model
 
variables <- data.frame(T,pH,aw)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T,pH,aw) {
   mumax <-exp(-6.001+0.2342*T+14.12*(sqrt(1-aw))-0.003552*(T^2)+0.005554*(pH^2)-76.68*((sqrt(1-aw))^2))

return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['pH'],argumentsPar['aw']))
colnames(responseSurface) <- c(colnames(argumentsPar),'lnmumax')
#############################
# End of Model script
#############################
