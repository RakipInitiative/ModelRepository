

#################################
#visualisation
#################################
xmin = min(Cretlog)
xmax = max(Cretlog)*1.1
ymin = min(log10(dosemean))
ymax = max(log10(dosemean))*1.1
plot(Cretlog, log10(dosemean), xlab = "log(C_ret) in cfu/g", ylab = "log(dose) in cfu/g after consumer handling of chicken meat in ready-to-eat chicken salad", main = "result of van Asselt Consumer phase model",xlim=c(xmin,xmax),ylim=c(ymin,ymax))


