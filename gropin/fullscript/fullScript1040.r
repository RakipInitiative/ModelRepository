#############################
# start of Parameter script
#############################
T_start <- 2.002
T_end <- 19.98001998002
T_fixed <- 10.99
pH_start <- 4.004
pH_end <- 7.49250749250749
pH_fixed <- 5.75
aw_start <- 0.95095
aw_end <- 0.994005994005994
aw_fixed <- 0.97
visVar1 <- 'pH'
visVar2 <- 'T'
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 1040 
#############################
# variables of this model
T <- seq(T_start, T_end ,length.out=21)
pH <- seq(pH_start, pH_end ,length.out=21)
aw <- seq(aw_start, aw_end ,length.out=21)
 
# constant coefficients for this model
 
variables <- data.frame(T,pH,aw)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T,pH,aw) {
   mumax <-exp(-6.001+0.2342*T+14.12*(sqrt(1-aw))-0.003552*(T^2)+0.005554*(pH^2)-76.68*((sqrt(1-aw))^2))

return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['pH'],argumentsPar['aw']))
colnames(responseSurface) <- c(colnames(argumentsPar),'lnmumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 1040 
#############################
visAxes <- c(visVar1,visVar2)
expectedAxes <- c('T','pH','aw')
checkPresent <- match(expectedAxes,visAxes)
missingAxis <- expectedAxes[is.na(checkPresent)]
if('T' %in% visAxes){
  ax1 <- T
} else {
  ax1 <- T_fixed
}

if('pH' %in% visAxes){
  ax2 <- pH
} else {
  ax2 <- pH_fixed
}

if('aw' %in% visAxes){
  ax3 <- aw
} else {
  ax3 <- aw_fixed
}

argumentsVisVar <- expand.grid(ax1,ax2,ax3)
colnames(argumentsVisVar) <- expectedAxes
firstChosenAxis <- expectedAxes[
  min(
    which(expectedAxes == expectedAxes[match(1,checkPresent)]),
    which(expectedAxes == expectedAxes[match(2,checkPresent)]))
]
secondChosenAxis <- expectedAxes[
  max(
    which(expectedAxes == expectedAxes[match(1,checkPresent)]),
    which(expectedAxes == expectedAxes[match(2,checkPresent)]))
]

result <- matrix(unlist(response_surface(
	argumentsVisVar['T'],	argumentsVisVar['pH'],	argumentsVisVar['aw']
)),nrow=21,byrow=F)

#order of entries in result is always:
# rows of the 1st chosen index of expectedAxes
# cols of the 2nd chosen index of expectedAxes
# this makes sure, the order of visVar1 and visVar2 are not relevant
rownames(result) <- unlist(variables[firstChosenAxis])
colnames(result) <- unlist(variables[secondChosenAxis])
persp(as.numeric(rownames(result)),
	as.numeric(colnames(result)),
	result,
	col = 'green',
	xlab=firstChosenAxis,
	ylab=secondChosenAxis,
	zlab='ln<mu>max',
	main='Response surface ln<mu>max for Gropin Model (ID 1040)',
	sub='Yarrowia lipolytica in/on Dairy products, Sugar solutions, beverages',
	theta=305,
	phi=20,
	shade=0.25,
	ticktype = 'detailed')
#############################
# End of Visualisation script
#############################
