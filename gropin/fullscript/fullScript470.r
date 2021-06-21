#############################
# start of Parameter script
#############################
T <- seq(-1.72172,44.955044955045,length.out=21)
pH <- seq(4.71471,9.6003996003996,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 470 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T,pH)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(T,pH) {
   mumax <-(-232.64+((1.4041*(10^5))/(T+273))+((-2.1908*10^7)/((T+273)^2))+(1.1586*(10^2)/pH)+((-4.0952*(10^2))/(pH^2)))

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['pH']))
colnames(responseSurface) <- c(colnames(argumentsPar),'lnmumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 470 
#############################
titleText <-'Response surface ln_mu_max for
Listeria monocytogenes in/on Beef _lean_
(gropin ID:470)'
persp(T,pH,matrix(unlist(responseSurface$'lnmumax'),nrow=21),col = 'green',xlab='T',ylab='pH',zlab='lnmumax',main=titleText,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
#############################
# End of Visualisation script
#############################
