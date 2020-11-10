#######################################
# Parameter skript
#######################################
iter <- 10000 # number of iterations for simulation
NPMmean <- -1.34 # mean of microbial concentration in pork meat
NPMsd <- 0.16 # sd of microbial concentration in pork meat
percPMmin <- 65 # min of percentage of pork meat in mixture
percPMmax <- 80 # max of percentage of pork meat in mixture
Wbatch <- 1000 # total weight of the mixture
prevTarget <- 0 # target number for micriobial load in mixture to calculate prevalence



#######################################
# Model skript
#######################################
#creating distribution from given parameters
NPMdist <- rnorm(iter,NPMmean,NPMsd)
percPMdist <- runif(iter,percPMmin,percPMmax)

# MC sim for microbial load
Nmix <- 10^NPMdist*percPMdist/100*Wbatch*1000

# unit changes to cfu/g
NmixG <- Nmix/(Wbatch*1000)

# prevalence calculations
NmixCum <- ecdf(Nmix)
p1 <- 1- NmixCum(prevTarget)
#######################################
# Visulisation skript
#######################################
library(gridBase)
library(gridExtra)
library(grid)


# prevalence as a table
myTable <-  as.data.frame(paste(p1*100,'%'))
rownames(myTable) <- 'Prevalence in the\nmixture tank'
colnames(myTable) <- 'p1'
mytheme <- gridExtra::ttheme_default(
  core = list(fg_params=list(cex = 1.0)),
  colhead = list(fg_params=list(cex = 1.0)),
  rowhead = list(fg_params=list(cex = 0.95)))
myTable <- gridExtra::tableGrob(myTable, theme = mytheme)


# bringing all results into 1 frame
par(mfrow=c(2,2))
hist(Nmix, 
     main='Total microbial load\nin the mixture',
     xlab='Nmix [cfu/mixture]')
hist(NmixG,
     main='Microbial concentration\nin the mixture',
     xlab='Nmix [cfu/g]')
hist(log(NmixG),
     main='Microbial concentration\nin the mixture',
     xlab='Nmix [log cfu/g]')
plot.new()
vps <- baseViewports()
pushViewport(vps$figure)
vp1 <-plotViewport()
grid.draw(myTable)
