#############################
# start of Model script
#############################
multVar1 <- T
multVar2 <- pH
 
response_surface <- function(T,pH) {
   (-232.64+((1.4041*(10^5))/(T+273))+((-2.1908*10^7)/((T+273)^2))+(1.1586*(10^2)/pH)+((-4.0952*(10^2))/(pH^2)))
} 
result <- outer(multVar1,multVar2,response_surface)
colnames(result)<-multVar2
rownames(result)<-multVar1
#############################
# End of Model script
#############################
