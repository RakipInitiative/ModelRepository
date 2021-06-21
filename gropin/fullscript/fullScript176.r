#############################
# start of Parameter script
#############################
T <- seq(6.006,30.969030969031,length.out=21)
pH <- seq(4.6046,9.29070929070929,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 176 
#############################
 
# constant coefficients for this model
a11 <- 22.87
pHsubmin <- 4.14
pHsupmax <- 9.8000000000000007
pHmin <- 4.26
pHmax <- 9.77
 
variables <- data.frame(T,pH)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(T,pH) {
   mumax <-(a11*(((pH-pHmin)*(pH-pHmax))/((pH-pHsubmin)*(pH-pHsupmax))))

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['pH']))
colnames(responseSurface) <- c(colnames(argumentsPar),'lnmumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 176 
#############################
titleText <-'Response surface ln_mu_max for
Lactobacillus curvatus in/on MRS broth
(gropin ID:176)'
persp(T,pH,matrix(unlist(responseSurface$'lnmumax'),nrow=21),col = 'green',xlab='T',ylab='pH',zlab='lnmumax',main=titleText,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
#############################
# End of Visualisation script
#############################
