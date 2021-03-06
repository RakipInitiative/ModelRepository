
#################################
#################################
#################################
# risk reduction
#################################
#################################
#################################

# CPMs	
# 1	Christensen
# 2	FAO/WHO
# 3	Mylius
# 4	Van Asselt
# 5	Nauta
# 6	Brynestad
# 7	Calistri
# 8	Lindqvist
# 
# DRs	
# 1	classic
# 2	median challenge
# 3	outbreak



CPM = 5
DRM = 2
#Cretlog <- rep(1,91)
#Pillmean <- rep(1,91)
niter <- 1000
slopeMin <- 0.0
slopeML <- 0.27
slopeMax <- 0.7
tauMin <- 0.0
tauMax <- 3.0
scenarioMeanUser <- 1.0
scenarioVariabilityUser <- 0.0
scenarioUncertaintyUser <- 0.0
# EU stuff
MS <- "EU"
myVis <- 0
#p <- 67.0
#mu <- 2.44
#sigma <- 1.24
#################################

library(MALDIquant)  
library(plyr)
library(mc2d)
#################################
# EFSA opinion model
#################################
# model 
#################################

CPMnames <- c("Christensen", 
              "FAO/WHO", 
              "Mylius", 
              "Van Asselt", 
              "Nauta", 
              "Brynestad", 
              "Calistri", 
              "Lindqvist")
DRMnames <- c("classic", 
              "median challenge", 
              "outbreak")

#random CPM/DRM combination as in @Risk code
if (CPM < 1 | CPM > length(CPMnames)) CPM <- round(runif(1,1,length(CPMnames)))
if (DRM < 1 | DRM > length(DRMnames)) DRM <- round(runif(1,1,length(DRMnames)))

# load CPM, DRM
filenamesDRM = c("allCPMsDRMclass.csv", "allCPMsDRMMedChall.csv", "allCPMsDRMMedOut.csv")

dfCPMDRM = read.table(paste0("~/PycharmProjects/ModelRepository/nauta_efsa_opinion/",filenamesDRM[DRM]), 
                      header = TRUE,
                      sep = ";")

Cretlog <- dfCPMDRM$Cretlog
Pillmean <- as.matrix(dfCPMDRM[CPM+1]) # because CPMs in csv start with column 2 end with column 9
# load from all Member states skin results 
dfMS <- read.table("~/PycharmProjects/ModelRepository/nauta_efsa_opinion/ms.csv", 
                 header = TRUE,
                 sep = ";")


getMSROW <- which(dfMS == MS, arr.ind = TRUE)[1]

if (is.na(getMSROW)) {
  getMSROW <- runif(1,1,nrow(dfMS))
  MS = dfMS$MS[getMSROW]
  }

p <- dfMS$pMS[getMSROW]
mu <- dfMS$muMS[getMSROW]
sigma <- dfMS$sigmaMS[getMSROW]


#load all reduction scenarios from EFSA opinion
dfRedScen <- read.table("~/PycharmProjects/ModelRepository/nauta_efsa_opinion/reductionscenarios.csv", 
                   header = TRUE,
                   sep = ";")

# machine tolerance for numeric error, needed for matching double numbers
tol = .Machine$double.eps^0.5

# case correction from other models
CretLog <- Cretlog


CretSkin <- c(0.0001,0.0002,0.0005,
              0.001,0.002,0.005,
              0.01,0.02,0.05,
              0.1,0.2,0.5,
              1,2,5,
              10,20,50,
              100,200,500,
              1000,2000,5000,
              10000,20000,50000,
              100000,200000,500000,
              1000000,2000000,5000000,
              10000000,20000000,50000000)

CretLogSkin <- log10(CretSkin)


# user interface
slope <- rpert(niter,slopeMin,slopeML,slopeMax)
#slope <- 0.27
tau <- runif(1, min = tauMin, max = tauMax)
#tau<-1



# DR plus
CretlogMeat <- CretLogSkin - tau

lowerLogRisk <- log10(Pillmean[match.closest(round_any(CretlogMeat, 0.1, f=floor),CretLog,tol=tol)])
lowerLogRisk <- ifelse(is.na(lowerLogRisk),0,lowerLogRisk)
upperLogRisk <- log10(Pillmean[match.closest(round_any(CretlogMeat, 0.1, f=ceiling),CretLog,tol=tol)])
upperLogRisk <- ifelse(is.na(upperLogRisk),0,upperLogRisk)

logRiskMod <- (lowerLogRisk-upperLogRisk)*(CretlogMeat%%0.1)*10
logRisk <- lowerLogRisk - logRiskMod


risk <- ifelse(CretlogMeat < -2, 0 , 10^logRisk)


#meat conc distributions
Pdose1 <- dnorm(CretLogSkin,mu,sigma)
delta <- diff(CretLogSkin)
logDoseInterval <- array(NA, length(CretLogSkin))
logDoseInterval[2:(length(CretLogSkin)-1)] <- (delta[1:length(delta)-1] + delta[2:length(delta)])/2

# Correction? in @Risk code even weirder
logDoseInterval[1] <- logDoseInterval[2]
logDoseInterval[length(CretLogSkin)] <- logDoseInterval[length(CretLogSkin)-1]

#H column
newVector <- Pdose1*logDoseInterval
Pdose <- newVector/sum(newVector)

# columns K through M
# here happens the actual simulation
RR <- matrix(NA,niter,nrow(dfRedScen)+1)
#parameter
for(scen in 1:(nrow(dfRedScen)+1)) {
  if (scen<=nrow(dfRedScen)) {
    scenarioMean <- dfRedScen$scenarioMean[scen]
    scenarioVar <- dfRedScen$scenarioVar[scen]
    scenarioUnc <- dfRedScen$scenarioUnc[scen]
  } else {
    scenarioMean <- scenarioMeanUser
    scenarioVar <- scenarioVariabilityUser
    scenarioUnc <- scenarioUncertaintyUser
  }
  deltaFec <- rnorm(niter,scenarioMean,scenarioUnc)
  muAfterIntervention <- mu - slope*deltaFec#
  sigmaAfterIntervention <- sqrt(sigma^2 + scenarioVar^2*slope^2)*rep(1,length(muAfterIntervention))#

  Pdose1AfterIntervention <- matrix(NA,length(CretLogSkin),niter)
  newVectorAfterIntervention <- matrix(NA,length(CretLogSkin),niter)

  for(i in 1:niter) {
    Pdose1AfterIntervention[1:length(CretLogSkin),i] <- dnorm(CretLogSkin,muAfterIntervention[i],sigmaAfterIntervention[i])
  }
  newVectorAfterIntervention <- matrix(logDoseInterval,length(CretLogSkin),niter)*Pdose1AfterIntervention
  PdoseAfterIntervention <- newVectorAfterIntervention/t(matrix(colSums(newVectorAfterIntervention),niter,length(CretLogSkin)))



  #back to DR plus
  RISK <- risk*Pdose
  RISKAfterIntervention <- risk*PdoseAfterIntervention

  RR[1:niter,scen] <- ifelse(colSums(RISKAfterIntervention)>sum(RISK),1,colSums(RISKAfterIntervention)/sum(RISK))
}



#################################


#################################
#visualization
#################################





RRdens <- matrix(NA,niter,nrow(dfRedScen))
RRerrMinus <- rep(NA,nrow(dfRedScen))
RRerrPlus <- rep(NA,nrow(dfRedScen))
RRmean <- rep(NA,nrow(dfRedScen))


if(myVis == 1) upTo<-nrow(dfRedScen) else upTo<-nrow(dfRedScen)+1
myQuant <- matrix(NA,upTo,7)

for(scen in 1:upTo){
   RRquant <- quantile(RR[1:niter,scen],c(.01,.025,.05,.5,.95,.975,.99))
   myQuant[scen,1:7] <- as.vector(RRquant)
   RR2p5 <- 1-RRquant["2.5%"]
   RR50 <- 1-RRquant["50%"]
   RR97p5 <- 1-RRquant["97.5%"]
   RRmean[scen] <- (1-mean(RR[1:niter,scen]))*100
   RRerrMinus[scen] <- (RR2p5)*100
   RRerrPlus[scen] <- (RR97p5)*100
  
   
   #RRdens[1:niter,scen] <- (1-RR[1:niter,scen])
}
RRleft <- RRmean+RRerrMinus
RRright <- RRmean-RRerrPlus

#################################

if (myVis==1) {
  library(gridExtra)
  library(gridGraphics)
  
  results <- t(myQuant)
  mylabel <- as.character(dfRedScen$Scenario[1:5])
  #mylabel <- append(mylabel,"User\n Scenario")
  rownames(results) <- names(RRquant)
  colnames(results) <- mylabel
  #pushViewport(viewport(x = .5, y = .4, height = .2, width = .2))    
  grid.table(results)	
}			

  
 
#################################



RRR <- (1-RR)*100

 # dfPlot <- cbind(FA1 = RRR[1:niter,1],
 #                 FA2 = RRR[1:niter,2],
 #                 FA3 = RRR[1:niter,3],
 #                VA1 = RRR[1:niter,4],
 #                 VA2 = RRR[1:niter,5],
 #                 UserScenario = RRR[1:niter,6])

myText = paste0("Uncertainty of relative risk reductions for different scenarios\n for member state: ",MS)
# boxplot(dfPlot, 
#         main = myText, 
#         ylab="Relative Risk Reduction (%)", ylim=c(0,100),
#         notch = TRUE, col = 2:7)
#plot(c(dfRedScen$Scenario[1:5]), RRmean, ylim=c(-3, 3), xlab="x", ylab="y", pch=16, cex=2)
#par(mfrow=c(2,1))
#grid.table(resultsCPM)
if (myVis==0) {
plot(RRmean,
     axes=FALSE, 
     ylim=c(0,100),
     xlim=c(0.5,6.5),
     main = myText, 
     xlab="Reduction Scenarios", 
     ylab="Relative Risk Reduction (%)", 
     pch=16, cex=2)
#axis(1, at=1:10, labels=dfRedScen$Scenario[1:5])
mylabel <- as.character(dfRedScen$Scenario[1:5])
mylabel <- append(mylabel,"User\n Scenario")
axis(2)
axis(1, at=seq_along(RRmean),labels=mylabel, las=2)
box()
arrows(x0=1:6, y0=RRerrMinus, x1=1:6, y1=RRerrPlus, code=3, angle=90, length=0.1)
text(5.5,95,family="A",font=2, paste0("CPM: ", CPMnames[CPM]), pos=3)
text(5.5,87,family="D",font=2, paste0("DRM: ", DRMnames[DRM]), pos=3)
}
#plot(RRdens, type="l", lwd=3, ylab=" ",xlab="Risk Reduction", main="probability of Risk Reduction")
# segments(RRleft,0, RRleft, RRdens$y[match.closest(RRleft,RRdens$x)], lwd=3, lty=2)
# segments(RRright,0, RRright, RRdens$y[match.closest(RRright,RRdens$x)], lwd=3, lty=2)
# segments(RRmean,0, RRmean, RRdens$y[match.closest(RRmean,RRdens$x)], lwd=3, lty=3)
# text(RRmean-0.1, RRdens$y[match.closest(RRmean,RRdens$x)]/10, paste0("Mean RRR=", round_any(RRmean*100,0.1), "%"), pos=3)




#################################
