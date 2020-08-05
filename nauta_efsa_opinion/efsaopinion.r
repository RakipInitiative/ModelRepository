#parameters

niter <- 250000 # iteration over all distributions
#dose <- rep(1,100000)
dose <- seq(-4,4,by=0.3)
#CretLog <- rep(1,100000)
CretLog <- seq(-2,2,by=0.1)

Pill <- seq(-3.5,-2.3,by=0.03)

CretLogSkin <- seq(-4,7.7,by=0.3)

# slope ~ pert(slopeMin,slopeML,slopeMax)
slopeMin <- 0
slopeML <- 0.27
slopeMax <- 0.7

# tau ~ unif(tauMin,tauMax)
tauMin <- 0 
tauMax <- 3

# scenario FA1 --> deltaFec ~ norm(scenarioMean,scenarioUnc)
scenarioMean <- 1.23
scenarioVar <- 0
scenarioUnc <- 0.66

#  MS skin results
p <- 80.6
mu <- 2.18
sigma <- 1.38
production <- 1.1

#model
library(plyr)
library(mc2d)

# user interface
slope <- rpert(1,slopeMin,slopeML,slopeMax)
tau <- runif(1, min = tauMin, max = tauMax)
tau <-0.1
deltaFec <- rnorm(1,scenarioMean,scenarioUnc)


# DR plus
CretlogMeat <- CretLogSkin - tau

CretLog <- round_any(CretLog,0.1,f=floor) # so that match works properly

lowerLogRisk <- Pill[match(round_any(CretlogMeat, 0.1, f=floor),CretLog)]
upperLogRisk <- Pill[match(round_any(CretlogMeat, 0.1, f=ceiling),CretLog)]

logRiskMod <- (lowerLogRisk-upperLogRisk)*(CretlogMeat%%0.1)*10
logRisk <- lowerLogRisk - logRiskMod

risk <- ifelse(CretlogMeat<-2,0,10^logRisk)


#meat conc distributions
Pdose1 <- dnorm(CretLogSkin,mu,sigma)
delta <- diff(CretLogSkin)
logDoseInterval <- array(NA, length(CretLogSkin))
logDoseInterval[2:length(CretLogSkin)-1] <- (delta[1:length(delta)-1] + delta[2:length(delta)])/2

# Correction? in @Risk code even weirder
logDoseInterval[1] <- logDoseInterval[2]
logDoseInterval[length(CretLogSkin)] <- logDoseInterval[length(CretLogSkin)-1]

#H column
newVector <- Pdose1*logDoseInterval
Pdose <- newVector/sum(newVector)

# columns K through M
muAfterIntervention <- mu - slope*deltaFec
sigmaAfterIntervention <- sqrt(sigma^2 + scenarioVar^2*slope^2)
Pdose1AfterIntervention <- dnorm(CretLogSkin,muAfterIntervention,sigmaAfterIntervention)
newVectorAfterIntervention <- logDoseInterval*Pdose1AfterIntervention
PdoseAfterIntervention <- newVectorAfterIntervention/sum(newVectorAfterIntervention)


#back to DR plus
RISK <- risk*Pdose
RISKAfterIntervention <- risk*PdoseAfterIntervention

RR <- ifelse(sum(RISKAfterIntervention)>sum(RISK),1,sum(RISKAfterIntervention)/sum(RISK))


RISKAFTER
#visualization


