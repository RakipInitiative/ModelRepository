############################# 
# start of Model script Gropin ID 161 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T,CitrA,AscorbA)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(T,CitrA,AscorbA) {
   mumax <-(0.1873+(0.7077*T)-(0.4681*CitrA)-(0.0706*AscorbA)+(0.03353*(T^2))+(0.5058*(CitrA^2))-(1.1107*T*CitrA)-(0.4981*T*AscorbA)+(0.3896*CitrA*AscorbA))

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['CitrA'],argumentsPar['AscorbA']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
