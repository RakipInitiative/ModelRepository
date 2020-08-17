MS <- "SI"


df <- read.table("/home/joker/PycharmProjects/ModelRepository/nauta_efsa_opinion/ms.csv", 
                 header = TRUE,
                 sep = ";")
getMSROW <- which(df == MS, arr.ind = TRUE)[1]

myP <- df$pMS[getMSROW]
mymu <- df$muMS[getMSROW]
mysigma <- df$sigmaMS[getMSROW]