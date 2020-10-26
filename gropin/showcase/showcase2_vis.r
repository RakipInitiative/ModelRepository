#############################
# start of Visualisation script
#############################

if(mode=='responsesurface') {
  plot(T,result,xlab='T',ylab='mu_max',
       main='Response surface mu_max for Gropin Model (ID 256)\n
        Staphylococcus aureus in/on Fresh milk')
}

if(mode=='time2multiply') {
  plot(T,time2Xlog,xlab='T',ylab='mu_max',
       main='Time in h to increase log step for Gropin Model (ID 256)\n
        Staphylococcus aureus in/on Fresh milk')
}

if(mode=='kinetic') {
  plot(t,logN,xlab='t in h',ylab='log N in CFU/g',
       main='Growth prediction of Gropin Model (ID 256)\n
        Staphylococcus aureus in/on Fresh milk')
}
#############################
# End of Visualisation script
#############################
