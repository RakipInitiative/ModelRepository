library("R2OpenBUGS")
setwd(print(dirname(sys.frame(1)$ofile)))

# Data specifications based on 2 published papers (Lindblad et al, Hansson et al).
NBpos=88
NBneg=529
SB=c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
     1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
     1,1,1,1,1,1,1,1,1,1,1,1,1,1)

logcfu=structure(.Data=c(
2.60,2.60,2.60,2.60,2.60,2.60,2.60,2.60,2.90,2.90,2.90,3.08,
3.08,3.20,3.26,3.34,3.40,3.41,3.51,3.51,3.56,3.56,3.60,3.64,
3.64,3.64,3.68,3.88,3.88,3.92,3.92,3.94,3.94,3.94,3.94,3.96,
3.97,4.01,4.02,4.02,4.06,4.07,4.09,4.10,4.16,4.16,4.18,4.19,
4.23,4.25,4.26,4.26,4.29,4.30,4.31,4.32,4.36,4.38,4.41,4.41,
4.50,4.60,4.60,4.64,4.65,4.68,4.70,4.71,4.71,4.76,4.77,4.82,
4.86,4.90,4.96,4.98,4.99,4.99,5.02,5.06,5.14,5.26,5.32,5.40,
5.42,5.42,6.17,7.15),.Dim=c(88,1))
Nbatches=20
mlogcfu=c(2.31,1.96,1.38,2.98,2.87,2.76,3.02,2.69,3.15,2.63,2.74,2.32,2.62,2.62,1.35,1.21,2.19,1.39,2.13,2.11)
sdlogcfu=c(0.61,0.51,0.60,0.48,0.71,0.39,0.58,0.40,0.49,0.37,0.37,0.26,0.49,0.35,0.81,0.80,0.48,0.75,0.69,0.61)
pos.carcass=c(24,10,21,16,12,13,5,10,20,11,20,15,17,17,17,18,23,20,19,11)
n.carcass=  c(25,10,23,16,12,13,5,10,20,11,20,15,17,17,20,21,23,20,20,11)
minuslogptr=c( 2.24, 2.36, 2.37, 2.58, 2.82, 2.86, 3.16, 3.17, 3.47, 3.52, 3.57, 3.83, 3.83, 3.84, 
3.87, 3.89, 3.89, 3.90, 3.94, 4.03, 4.09, 4.42, 4.53, 4.54, 4.54, 4.62, 4.62, 4.68, 4.73, 4.76, 4.84, 
4.92, 4.93, 4.95, 4.97, 5.20, 5.25, 5.27, 5.39, 5.47, 5.60, 5.83, 5.89, 5.95, 5.96, 6.02, 6.23, 6.38, 
6.96, 7.37, 7.90, 8.20, 9.00, 9.00, 9.00)
SDL <- 0.8549299  # SD in Lindblad data
meanL <- 2.112727  # mean in Lindblad data


# full model with RR (and mrrr)
repsB <- 40; repsS <- 10; #adjust 2D Monte Carlo: repsB=number of replicated batches, repsS=number of replicated servings
MCn <- 5; MCc <- 0; MCm <- 3  # Microbiological Criterion used. "n/c/m"
data <- list("repsB",
             "repsS",
             "MCn",
             "MCc",
             "MCm",
             "NBpos",
             "NBneg",
             "SB",
             "logcfu",
             "Nbatches",
             "mlogcfu",
             "sdlogcfu",
             "pos.carcass",
             "n.carcass",
             "minuslogptr")

parameters <- c("rr",
                "mrrr",
                "q",
                "mu",
                "sigma_b",
                "sigma_w",
                "phi",
                "PMCmet",
                "P.ill",
                "mP.ill")

inits <- function(){
  list(campycarcasses=rep(MCn,repsB),
       NC=structure(.Data=rep(2,repsB*repsS),.Dim=c(repsB,repsS)),
       Lmub=runif(88,1,2),
       Hmub=runif(20,1,2),
       mu=1,
       tau_w=3,
       tau_b=3,
       q=0.14,
       apw=10,
       pwithin=rbeta(20,9,1),
       ptrb=2000)
  }

resM5_03 <- bugs(data,
                 inits,
                 parameters,
                 "fullmodelAsup.txt",
                 n.chains=1,
                 n.iter=60,
                 n.burnin=10,
                 n.thin=2)

attach.bugs(resM5_03)
plot(1-PMCmet,
     rr,
     cex=0.5,
     pch=16,
     main="Criterion: n=5,c=0,m=3",
     xlab="P(MC not met) = %rejected batches",
     ylab="RR",
     col=densCols(1-PMCmet,rr,colramp=colorRampPalette(c("darkgreen","yellow","red"))))

