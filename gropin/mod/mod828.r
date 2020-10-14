#############################
# start of Model script
#############################
multVar1 <- pH
multVar2 <- T
 
response_surface <- function(pH,T) {
   1.035*((b2-o2)*(b2-k2)/((m2-k2)*(b2-m2)-(m2-o2)*(k2-b2)))*((c2-u2)*((c2-q2)^2)/((s2-q2)*((s2-q2)*(c2-s2)-(s2-u2)*(s2+q2-2*c2))))
} 
result <- outer(multVar1,multVar2,response_surface)
colnames(result)<-multVar2
rownames(result)<-multVar1
#############################
# End of Model script
#############################
