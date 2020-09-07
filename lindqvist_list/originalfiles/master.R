#Master script to make 250 number of simulations of the mgQMRA script (The number of iterations defined in
#that script. Results vary within one simulation due to random variations in sampling, especially the tails
#of the distribution. Since the predicted risk is quite low this can affect outcome and ranking quite a lot. 
#Due to memeory constraints the maximum number of iterations is limited so to obtain stable results multiple 
#simulations are carried out and ranking is based on multiple simulations

#The only user defined data to change in this file is the working directory and the 
#names of the output files

# Define a working directory. The needed files should be stored in this directory
setwd("~/Projects/ModelRepository/lindqvist_list/workdir/")
set.seed(3333) #Arbitrary number but should be set so differences are due only to differencs in input data

sims <- 250 #The number of simulations of script mgQMRA 
Pill_out <- lapply(1:sims, function(n)source("m.g.QMRA.R")$value[,1])

Pill_out #Probability of illness per serving
Pill_df<- data.frame(matrix(unlist(Pill_out), ncol=length(Pill_out), byrow=F))

Pill_df <- data.frame(t(Pill_df))
colnames(Pill_df) <- c("csfR","hsfR","grfR","come","sausR","pateR","fvR","csf","hsf","grf","comeR","saus","pate","sssch","fvco")
Pill_df #Data frame with the probability of illness per serving per simulation per food subcategory

#Write the mean probability of illness per serving for the food_category for each simulation 
write.xlsx(Pill_df, "results", sheetName="scenario_name", append=TRUE)

#Description of the outcome of the 250 simulations per food category
perc_quant <- function(x) {
  quantile(x, probs = c(0.025, 0.5, 0.975))
}

Pserv_res <- as.data.frame(sapply(Pill_df[, 1:15], perc_quant)) #15 food subcategories

Pserv_res

pserv_mean <- sapply(Pill_df[, 1:15], mean)
pserv_sd <- sapply(Pill_df[, 1:15], sd)

Pserv_res <- rbind(Pserv_res, mean=pserv_mean)
Pserv_res <- rbind(Pserv_res, sd=pserv_sd)
Pserv_res

###Ranking based on the mean probability per serving
pop.group #population according to indata.file (65-74, females=11, 65-74, males=12)

#Ranking based on mean probability of illness per serving
Pill <- as.data.frame(t(Pserv_res))
Pill_mean_r <- Pill[order(-Pill$mean),]
Pill_mean_r

#Write rankings of the food subcateregories based on the mean probability of illness per serving 
write.xlsx(Pill_mean_r, "results2", sheetName="scenario_name", append=TRUE)
