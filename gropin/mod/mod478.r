#############################
# start of Model script
#############################
multVar1 <- aw
multVar2 <- NO2
 
response_surface <- function(aw,NO2) {
   1150.5+(-2330.2)*aw+(-0.58142)*((NO2*1000)/(69.01*(1+10^(6.1-3.37))))+0.33406*aw*((NO2*1000)/(69.01*(1+10^(6.1-3.37))))+1182.1*(aw^2)+(-0.019875)*(((NO2*1000)/(69.01*(1+10^(6.1-3.37))))^2)
} 
result <- outer(multVar1,multVar2,response_surface)
colnames(result)<-multVar2
rownames(result)<-multVar1
#############################
# End of Model script
#############################
