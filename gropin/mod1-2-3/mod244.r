############################# 
# start of Model script Gropin ID 244 
#############################
 
# constant coefficients for this model
a0 <- -10.0315
ax <- 0.324918
ax2 <- -6.7600000000000004E-3
ay <- 1.048619
ay2 <- -0.12821
abw <- 56.47651
abw2 <- -156.337
axy <- 0.015621
axbw <- 0.358186
aybw <- -2.93541
 
variables <- data.frame(T,pH,aw)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T,pH,aw) {
   mumax <-a0+(ax*T)+(ax2*(T^2))+(ay*pH)+(ay2*(pH^2))+(abw*sqrt(1-aw))+(abw2*(sqrt(1-aw)^2))+(axy*T*pH)+(axbw*T*sqrt(1-aw))+(aybw*pH*sqrt(1-aw))

return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['pH'],argumentsPar['aw']))
colnames(responseSurface) <- c(colnames(argumentsPar),'lnmumax')
#############################
# End of Model script
#############################
