#############################
# start of Parameter script
#############################
Sugar <- seq(66.066,69.9300699300699,length.out=21)
pH <- seq(2.002,3.4965034965035,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 1062 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(Sugar,pH)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(Sugar,pH) {
   mumax <-12.63699-0.50885*Sugar+0.00459363*(Sugar^2)+4.26298*pH-0.038048*(pH^2)-0.05637*Sugar*pH

return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['Sugar'],argumentsPar['pH']))
colnames(responseSurface) <- c(colnames(argumentsPar),'lnmumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 1062 
#############################
persp(Sugar,pH,matrix(unlist(responseSurface$'lnmumax'),nrow=21),col = 'green',xlab='Sugar',ylab='pH',zlab='lnmumax',main='Response surface lnmumax for
Zygosaccharomyces rouxii in/on Apple juice concentrated
(gropin ID:1062)',theta=305,phi=20,shade=0.25,ticktype = 'detailed')
#############################
# End of Visualisation script
#############################
