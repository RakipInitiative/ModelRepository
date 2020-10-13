#############################
# start of Parameter script
#############################
aw <- seq(0.895895,0.981018,length.out=21)
awmin <- 0.895
awopt <- 0.988
#############################
# end of Parameter script
#############################
 
#############################
# start of Model script
#############################
 
response_surface <- function(aw) {
   (9.97^0.5)*(((aw-1)*(aw-awmin)^2)/((awopt-awmin)*(((awopt-awmin)*(aw-awopt))-((awopt-1)*(awopt+awmin-2*aw)))))^0.5
} 
result <- response_surface(aw)
#############################
# End of Model script
#############################
 
#############################
# start of Visualisation script
#############################
plot(aw,result,xlab='aw',ylab='mu_max')
#############################
# End of Visualisation script
#############################
