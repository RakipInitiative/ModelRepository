#############################
# start of Model script
#############################
multVar1 <- pH
multVar2 <- NO2
 
response_surface <- function(pH,NO2) {
   8.0091+(-0.87877)*pH+0.42624*((NO2*1000)/(69.01*(1+10^(pH-3.37))))+0.18149*pH*((NO2*1000)/(69.01*(1+10^(pH-3.37))))+(-0.013099)*(pH^2)+(-0.20018)*(((NO2*1000)/(69.01*(1+10^(pH-3.37))))^2)
} 
result <- outer(multVar1,multVar2,response_surface)
colnames(result)<-multVar2
rownames(result)<-multVar1
#############################
# End of Model script
#############################
