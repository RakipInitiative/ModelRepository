t <- seq(0,60,length.out = 71)

logNmax <- 10
logN0 <- 1
mumax <- 0.492105
lag <- 1
q0 <- 1/(exp(lag)-1)
A = t + (1/mumax)*log((exp(-mumax*t)+q0)/(1+q0))

logN = logN0 + mumax*A - log(1+((exp(mumax*A)-1)/(exp(logNmax-logN0))))

plot(t,logN)