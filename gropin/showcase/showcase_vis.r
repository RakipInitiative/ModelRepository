#############################
# start of Visualisation script
#############################
if(mode=='responsesurface') {
  persp(as.double(values(myHash[visVar1])),
                 as.double(values(myHash[visVar2])),
                 result,
                 col = 'green',
                 xlab=keys(myHash[visVar1]),
                 ylab=keys(myHash[visVar2]),
                 zlab='mu_max',
                 main='response surface mu_max for Gropin Model Nr. 24 ',
                 theta=35,
                 phi=20,
                 shade=0.25,
                 ticktype = 'detailed')
}
if(mode=='time2multiply') {
                 
  myZ <- paste('time to increase',logIncrease,'step(s)')
                 
  persp(as.double(values(myHash[visVar1])),
                 as.double(values(myHash[visVar2])),
                 time2Xlog,
                 col = 'green',
                 xlab=keys(myHash[visVar1]),
                 ylab=keys(myHash[visVar2]),
                 zlab=myZ,
                 main='Time in h to increase log step for Gropin Model Nr. 24 ',
                 theta=35,phi=20,shade=0.25,ticktype = 'detailed')
}
if(mode=='kinetic') {
               
  plot(t,logN,
               xlab='t in h',
               ylab='log N in CFU/g',
               main='Gropin Model Nr. 24 ')
               
}
#############################
# End of Visualisation script
#############################
