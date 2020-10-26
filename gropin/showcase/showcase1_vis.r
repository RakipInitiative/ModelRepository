#############################
# start of Visualisation script
#############################
if(mode=='responsesurface') {
  persp(multVar1,
        multVar2,
        result,
        col = 'green',
        xlab=visVar1,
        ylab=visVar2,
        zlab='mu_max',
        main='Response surface mu_max for Gropin Model (ID 24)\n
        Aeromonas hydrophilia in/on modified BHI',
        theta=35,
        phi=20,
        shade=0.25,
        ticktype = 'detailed')
}
if(mode=='time2multiply') {
                 
  myZ <- paste('time to increase',logIncrease,'step(s)')
                 
  persp(multVar1,
        multVar2,
        time2Xlog,
        col = 'green',
       xlab=visVar1,
       ylab=visVar2,
       zlab=myZ,
       main='Time in h to increase log step for Gropin Model (ID 24)\n
        Aeromonas hydrophilia in/on modified BHI',
       theta=35,phi=20,shade=0.25,ticktype = 'detailed')
}
if(mode=='kinetic') {
               
  plot(t,logN,
       xlab='t in h',
       ylab='log N in CFU/g',
       main='Growth prediction of Gropin Model (ID 24)\n
        Aeromonas hydrophilia in/on modified BHI')
               
}
#############################
# End of Visualisation script
#############################
