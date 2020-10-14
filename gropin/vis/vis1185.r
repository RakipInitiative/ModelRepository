#############################
# start of Visualisation script
#############################
persp(as.double(values(myHash[visVar1])),as.double(values(myHash[visVar2])),result,col = 'green',xlab=keys(myHash[visVar1]),ylab=keys(myHash[visVar2]),zlab='mu_max',theta=35,phi=20,shade=0.25,ticktype = 'detailed')
#############################
# End of Visualisation script
#############################
