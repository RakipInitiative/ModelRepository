# Define a working directory. The needed files should be stored in this directory
#setwd("~/Projects/ModelRepository/lindqvist_list/workdir/")
set.seed(3333) #Arbitrary number but should be set so differences are due only to differencs in input data

sims <- 2 #The number of simulations of script mgQMRA 
#Pill_out <- lapply(1:sims, function(n)source("~/Projects/ModelRepository/lindqvist_list/originalfiles/m.g.QMRA.R")$value[,2])


testthis <- function(mytest,tey){
  mytest <- mytest + tey
  return(list(mytest=mytest))
}

runtest <- testthis(1,2)
