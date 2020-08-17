# CPM

#################################
#visualisation
#################################
library(ggridges)
library(ggplot2)
library(viridis)

# if the user restricts the number of dose distributions shown:
# dhelp becomes a subset of dose
# chelp becomes a subset of Cretlog
dhelp <- dose
chelp <- Cretlog

totalNrOfDists <- length(Cretlog)

# take 1st and last Cretlog and a number of distributions in between
if (totalNrOfDists>maxDistsShown){
  totalNrOfDists = length(Cretlog)
  
  stepSize = round((totalNrOfDists-1)/(maxDistsShown-1))
  steps = seq(1,totalNrOfDists,by=stepSize)
  
  #consistency check
  if(length(steps)<maxDistsShown) maxDistsShown <- length(steps)
  
  steps[maxDistsShown] <- length(Cretlog)
  
  chelp <- Cretlog[steps]
  dhelp <- dose[steps,1:niter]
}
dvec <- as.vector(t(log10(dhelp+0.001)))
cvec <- sort(rep(chelp,niter))
d <- data.frame(cvec,dvec)
colnames(d)<-c("log of contamination at retail (log C_ret)","log of dose of contamination at consumer")
ggplot(d, aes(x = `log of dose of contamination at consumer`, y = `log of contamination at retail (log C_ret)`, fill = ..x.., group=`log of contamination at retail (log C_ret)`)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis(name = "color scale", option = "C") +
  labs(title = 'doses of campylobacter in chicken salad as function of contamination of chicken meat bought at retail')

library(gridExtra)
library(gridGraphics)
resultsCPM <- cbind(round(dosemean,0), round(PrevExp*100,1))
rownames(resultsCPM) <- myCPMname
colnames(resultsCPM) <- c("Mean dose", 'Prevalence of exposure [%]')


tableCPM <- matrix(round(dose,0))

rownames(tableCPM) <- rownames(tableCPM, do.NULL=FALSE, prefix="Value.")
colnames(tableCPM) <- c('Dose')
#write.csv(tableCPM, file="doses-consumer-phase-Nauta.csv")

pushViewport(viewport(x = .6, y = .2, height = .2, width = .2))    
grid.table(resultsCPM)										


