
#visualisation
library(gridExtra)

results <- cbind(round(runDoseResponseMedianChallenge$PrevExp*100,1),
                 round(runDoseResponseMedianChallenge$Qill*100,2))
rownames(results) <- c("MedianChallenge DR")
colnames(results) <- c('Prevalence of Exposure [%]', 'Q_ill [%]')
grid.table(results)