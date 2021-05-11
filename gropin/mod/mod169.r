#############################
# start of Model script
#############################
 
variables <- data.frame(aw,notused)
argumentsPar <- expand.grid(variables)
response_surface <- function(aw,notused) {
   mumax <-matrix(unlist((2.86^0.5)*(((aw-1)*(aw-awmin)^2)/((awopt-awmin)*(((awopt-awmin)*(aw-awopt))-((awopt-1)*(awopt+awmin-2*aw)))))^0.5,nrow=21))
return(mumax=mumax)
} 
mumax <- response_surface(argumentsPar['aw'],argumentsPar['notused'])
#############################
# End of Model script
#############################
