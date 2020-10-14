#############################
# start of Parameter script
#############################
T <- seq(6.006,30.969,length.out=21)
pH <- seq(4.6046,9.2907,length.out=21)
a11 <- 22.87
pHsubmin <- 4.14
pHsupmax <- 9.8000000000000007
pHmin <- 4.26
pHmax <- 9.77
#############################
# end of Parameter script
#############################
 
#############################
# start of Model script
#############################
multVar1 <- T
multVar2 <- pH
 
response_surface <- function(T,pH) {
   (a11*(((pH-pHmin)*(pH-pHmax))/((pH-pHsubmin)*(pH-pHsupmax))))
} 
result <- outer(multVar1,multVar2,response_surface)
colnames(result)<-multVar2
rownames(result)<-multVar1
#############################
# End of Model script
#############################
 
#############################
# start of Visualisation script
#############################
persp(multVar1,multVar2,result,col = 'green',xlab='T',ylab='pH',zlab='mu_max',theta=35,phi=20,shade=0.25,ticktype = 'detailed')
#############################
# End of Visualisation script
#############################
