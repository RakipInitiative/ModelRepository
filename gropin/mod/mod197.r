############################# 
# start of Model script Gropin ID 197 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T,NaCl,pH,NaNO2)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(T,NaCl,pH,NaNO2) {
   mumax <-19.8265-(0.2028*T)+(0.0314*NaCl)-(4.3495*pH)+(0.0245*NaNO2)+(0.0000706*T*NaCl)-(0.0032*T*pH)+(0.0000162*T*NaNO2)-(0.00181*NaCl*pH)-(0.00000265*NaCl*NaNO2)-(0.00321*pH*NaNO2)+(0.00312*(T^2))+(0.0000258*(NaCl^2))+(0.3105*(pH^2))-(0.00000048*(NaNO2^2))

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['NaCl'],argumentsPar['pH'],argumentsPar['NaNO2']))
colnames(responseSurface) <- c(colnames(argumentsPar),'lnmumax')
#############################
# End of Model script
#############################
