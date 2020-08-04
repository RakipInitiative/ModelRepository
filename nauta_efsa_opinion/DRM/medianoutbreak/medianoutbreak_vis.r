
#visualisation
library(gridExtra)

results <- cbind(round(runDoseResponseMedianOutbreak$PrevExp*100,1),
                 round(runDoseResponseMedianOutbreak$Qill*100,2))
rownames(results) <- c("MedianOutbreak DR")
colnames(results) <- c('Prevalence of Exposure [%]', 'Q_ill [%]')
grid.table(results)