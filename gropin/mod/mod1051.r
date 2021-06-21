############################# 
# start of Model script Gropin ID 1051 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(pH,TA,Sugar,SB,PS)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(pH,TA,Sugar,SB,PS) {
   mumax <-(-42.43177+24.97906*pH+46.76338*TA-0.77329*Sugar-0.03119*PS-0.00790*SB-2.70679*(pH^2)-14.36986*pH*TA+0.00926*PS*pH+0.00179*PS*Sugar-0.00004*(PS^2)-0.00005*SB*PS+0.00002*(SB^2))

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['pH'],argumentsPar['TA'],argumentsPar['Sugar'],argumentsPar['SB'],argumentsPar['PS']))
colnames(responseSurface) <- c(colnames(argumentsPar),'lnmumax')
#############################
# End of Model script
#############################
