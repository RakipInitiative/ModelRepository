#############################
# start of Model script
#############################
multVar1 <- T
multVar2 <- aw
 
response_surface <- function(T,aw) {
   26.37*((((T-9.11)^2)*(T-46.45))/((32.11-9.11)*((32.11-9.11)*(T-32.11)-(32.11-46.45)*(32.11+9.11-2*T))))*((((aw-0.893)^2)*(aw-0.993))/((0.985-0.893)*((0.985-0.893)*(aw-0.985)-(0.985-0.993)*(0.985+0.893-2*aw))))
} 
result <- outer(multVar1,multVar2,response_surface)
colnames(result)<-multVar2
rownames(result)<-multVar1
#############################
# End of Model script
#############################
