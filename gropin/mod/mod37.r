#############################
# start of Model script
#############################
multVar1 <- T
multVar2 <- aw
 
response_surface <- function(T,aw) {
   (mopt*(((T-Tmax)*((T-Tmin)^2))/((Topt-Tmin)*((Topt-Tmin)*(T-Topt)-(Topt-Tmax)*(Topt+Tmin-2*T))))*(((aw-1)*(aw-awmin)^2)/((awopt-awmin)*((awopt-awmin)*(aw-awopt)-(awopt-1)*(awopt+awmin-2*aw)))))
} 
result <- outer(multVar1,multVar2,response_surface)
colnames(result)<-multVar2
rownames(result)<-multVar1
#############################
# End of Model script
#############################
