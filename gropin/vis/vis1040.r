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
