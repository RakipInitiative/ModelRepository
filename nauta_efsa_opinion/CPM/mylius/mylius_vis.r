
# visualization
library(gridExtra)

resultsCPM <- cbind(round(dosemean,0), round(PrevExp*100,1))
rownames(resultsCPM) <- c("CPM Mylius")
colnames(resultsCPM) <- c("Mean dose", 'Prevalence of exposure [%]')

tableCPM <- matrix(round(dose,0))
rownames(tableCPM) <- rownames(tableCPM, do.NULL=FALSE, prefix="Value.")
colnames(tableCPM) <- c('Dose')
write.csv(tableCPM, file="doses-consumer-phase-Mylius.csv")

sortdose <-sort(log10(dose))
vsize <-length(sortdose)
pdose <- array(NA,vsize)
for (i in 1:vsize)
{pdose[i] <- (i-0.5)/vsize}

plot(sortdose,pdose, 
     xlab = "log Dose", ylab = "",
     main = "doses from meat contaminated at retail")
grid.table(resultsCPM)