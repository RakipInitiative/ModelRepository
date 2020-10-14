#############################
# start of Model script
#############################
multVar1 <- T
multVar2 <- aw
 
response_surface <- function(T,aw) {
   28.03*((((T-10.58)^2)*(T-43.21))/((32.13-10.58)*((32.13-10.58)*(T-32.13)-(32.13-43.25)*(32.13+10.58-2*T))))*((((aw-0.892)^2)*(aw-0.992))/((0.984-0.892)*((0.984-0.892)*(aw-0.984)-(0.984-0.992)*(0.984+0.892-2*aw))))
} 
result <- outer(multVar1,multVar2,response_surface)
colnames(result)<-multVar2
rownames(result)<-multVar1
#############################
# End of Model script
#############################
