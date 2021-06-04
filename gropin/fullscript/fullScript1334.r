#############################
# start of Parameter script
#############################
pH <- seq(7.007,34.965034965035,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 1334 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(pH)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(pH) {
   mumax <-0.023*(pH-0.60)

return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['pH']))
colnames(responseSurface) <- c(colnames(argumentsPar),'Sqrmumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 1334 
#############################
plot(pH,responseSurface$'Sqrmumax',xlab='pH',
                          ylab='Sqrmumax',main='Response surface Sqrmumax for
Listeria monocytogenes in/on Leafy greens
(gropin ID:1334)')
#############################
# End of Visualisation script
#############################
