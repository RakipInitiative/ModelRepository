###################################################
### Parameter set 
###################################################
# default
sims <- 100
pop <- 11
runs <- 1000
shift <- 0.0
meanTemp <- 5.9
sdTemp <- 2.9
Mode_prop_rtime <- 0.3
Max_prop_rtime <- 1.1
lowerTemp <- -2.0
upperTemp <- 15.0
Tmin <- -1.18
###################################################
# full simulation run
sims <- 200
pop <- 11
runs <- 200000
shift <- 0.0
meanTemp <- 5.9
sdTemp <- 2.9
Mode_prop_rtime <- 0.3
Max_prop_rtime <- 1.1
lowerTemp <- -2.0
upperTemp <- 15.0
Tmin <- -1.18
#############################
# Model Script
#############################
###########################################
#Libraries used in the script
# library(readr)
# library(readxl)
library(msm)
# library(tidyverse)
# library(plotly)
library(mc2d)
# library(knitr)
# library(ggplot2)
# library(readr)
# library(dplyr)
# library(xlsx)
# library(Matrix)
setSessionTimeLimit(cpu=Inf, elapsed = Inf)
setTimeLimit(cpu=Inf, elapsed = Inf)
set.seed(3333) #Arbitrary number but should be set so differences are due only to differencs in input data

##########################################
# START imported script: m.g.QMRA.R


##Simplified gQMRA script: with the purpose of addding frozen food as heat treated or RTE, 
#extract results per food sucategory and evaluating only for one population at th time. 
#The possibility of inlcuding a simple heating step, log reduction is included. Risk is calculated 
#directly per food subcagegory, not for whole dose distributions per population. 
#This differes from teh original scipt, otherswise the same code and input data is used
#This script is run from the  master script to eanlbe multiple simulations

#User defined data to change in this script are:
#the name and data in the input data file. Input data can be changed to evalauate uncertainty
#the number of the population to be evaluated - defined in input data
#The prevalence estimate to be used for frozen vegetables - to evalaute uncertainty of this parameter



#################################################
#Extract input data to xfile
#################################################
#xfile="~/PycharmProjects/ModelRepository/lindqvist_list/originalfiles/input.data.xlsx"
#prev=read_excel(xfile,sheet="prev", col_names=TRUE)
#conc=read_excel(xfile,sheet="conc", col_names=TRUE)
#EGR=read_excel(xfile,sheet="EGR", col_names=TRUE)
#ROP=read_excel(xfile,sheet="ROP", col_names=TRUE)
#DRP=read_excel(xfile,sheet="DR", col_names=TRUE)
#conso=read_excel(xfile,sheet="conso", col_names=TRUE)
#size=read_excel(xfile,sheet="size", col_names=TRUE)
#r_time=read_excel(xfile,sheet="r_time", col_names=TRUE)
#log_red=read_excel(xfile,sheet="log_red", col_names=TRUE) #Log reduction beta-pert disribution

prev <- read.table("prev.data.csv", header=TRUE, sep=";", encoding="UTF-8")  
conc <- read.table("conc.data.csv", header=TRUE, sep=";", encoding="UTF-8")
EGR <- read.table("EGR.data.csv", header=TRUE, sep=";", encoding="UTF-8")  
ROP <- read.table("ROP.data.csv", header=TRUE, sep=";", encoding="UTF-8")  
DRP <- read.table("DR.data.csv", header=TRUE, sep=";", encoding="UTF-8")
conso <- read.table("conso.data.csv", header=TRUE, sep=";", encoding="UTF-8")  
size <- read.table("size.data.csv", header=TRUE, sep=";", encoding="UTF-8")  
r_time <- read.table("r_time.data.csv", header=TRUE, sep=";", encoding="UTF-8")  
log_red <- read.table("log_red.data.csv", header=TRUE, sep=";", encoding="UTF-8")  

modelPill_out <- function(sims, pop, runs,
                          prev, conc, EGR, ROP, DRP, conso, size, r_time, log_red){
  #################################################
  #################################################
  #Definition of functions needed
  #################################################
  
  
  pop.group <- DRP[pop,1:2]
  #pop.group
  
  #Convert mean and variance of Y to 
  #mean and std of log(Y) - to use rtnorm to get lognorm, later backtransformed in EGR5
  convert_m_v=function(m,v){
    phi = sqrt(v + m^2)
    mu=log(m^2/phi) #mean of log(Y)  
    sigma = sqrt(log(phi^2/m^2))#
    c(mu,sigma)
  }
  #Primary growth model
  rosso=function(time,egrm,lag=0,x0,xmax){ 
    x0=10^x0 
    xmax=10^xmax
    den=1+(xmax/x0 -1)*exp(-egrm*(time-lag))
    log10(xmax/den)
  } 
  #################################################
  #################################################
  ####Start of the definition of contamfunc
  ####shift is argument that shifts the xmax (maximum population density)
  ### meanTemp and sdTemp are the parameters of the normal distribution of 
  ###the consumer refrigerators
  ###Mode_prop_rtime and Max_prop_rtime are respectively the mode and the maximum of the proportion 
  ###of the shelflife used as time of storage
  ###runs is the number of iterations
  contamfun=function(runs,shift=shift,meanTemp=meanTemp,
                     sdTemp=sdTemp,
                     Mode_prop_rtime=Mode_prop_rtime,
                     Max_prop_rtime=Max_prop_rtime) {
    ######################################
    #Initial concentration for each
    #of the 15 RTE subcategories
    ######################################
    C0fun=function(i) rbetagen(runs, 
                               shape1=conc$shape1[i], 
                               shape2=conc$shape2[i], 
                               min=conc$min[i],  
                               max=conc$max[i]+shift) 
    
    #the function C0fun is applied to each of the fifteen
    #food categories thanks to sapply
    C0r=sapply(1:nrow(conc),C0fun)
    #C0r is a matrix with a number of lines equal to runs
    #and a number of columns eaqual to the number of food categories
    #dimension is runs x 15
    ######################################
    #Exponential Growth Rate
    #for each of the 15 food subcategories
    ######################################
    
    EGR5fun=function(i){
      m=EGR$m[i]
      s=EGR$sd[i]
      parm=convert_m_v(m,s^2)
      rtnorm(runs,mean=parm[1],sd=parm[2],
             lower=log(EGR$min[i]),upper=log(EGR$max[i])) 
    }
    
    EGR5r=exp(sapply(1:nrow(conc),EGR5fun))
    #dimension of EGR5r is runs x 15
    ######################################
    #Refrigerator Temperature
    #EGR
    ######################################
    
    Tempr=rtnorm(runs,mean=meanTemp,sd=sdTemp,
                 lower=lowerTemp,upper=upperTemp)
    #dimension of Tempr is runs x 1
    #Tmin=-1.18
    EGRr=EGR5r*((Tempr-Tmin)/(5-Tmin))^2
    EGRr[Tempr<Tmin]<-0
    
    #dimension of EGRr is runs x 15
    ######################################
    #Time of storage
    #
    ######################################
    
    ##########
    #remaining shelf life
    ##########
    r_timefun=function(i){
      m=r_time$m[i]
      pmin(rexp(runs,rate=1/m), r_time$max[i])
    }
    r_timer=sapply(1:length(unique(r_time$group)),r_timefun)
    #dimension of r_timer is runs x 15
    #########
    #Proportion of r_time
    #########
    
    propr=rpert(runs, min=0, mode=Mode_prop_rtime, max=Max_prop_rtime)
    #dimension of propr is runs x 1
    #########
    #s_time
    #########
    s_time=r_timer*propr
    #dimension of s_time runs x 15
    
    #########################################################
    #function describing heat inactivation (log reduction) 
    #during cooking (frozen vegetables)
    #fraction described by ROP in file for frozen vegetables
    #########################################################
    
    LRD=function(i) {
      reds <- f_concr[,i] - rpert(runs, min=log_red$min[i], mode=log_red$mode[i], max=log_red$max[i])
    }
    #Dimension is runs x 15
    
    
    ######################################
    #Concentration at time of consumption
    #f_conc
    ######################################
    f_concfun=function(i){
      Nmax=EGR$Nmax.mean[i]+shift
      rosso(s_time[,i],EGRr[,i],C0r[,i],lag=0,Nmax)
    }
    f_concr=sapply(1:length(unique(r_time$group)),f_concfun)
    ff_concr= sapply(1:length(unique(r_time$group)), LRD)
    #return(ff_concr)
  }
  
  #test of output
  resc=contamfun(runs=runs,shift=shift,meanTemp=meanTemp,
                 sdTemp=sdTemp,
                 Mode_prop_rtime=Mode_prop_rtime,
                 Max_prop_rtime=Max_prop_rtime)
  
  #simulated log contamination per food category
  ###head(resc) #run if want to inspect if reasonable output
  
  #simulated log dose per food category
  sim.dose <- function(runs, contam, pop){
    ss <- log10(as.numeric(as.vector(t(size[pop, 3:ncol(size)]))))
    dose <- ss + contam
  } 
  
  dose <- sim.dose(runs=runs, contam=resc, pop=pop)
  
  #Stochastic dose response model as in Pouillot et al. (2015)
  sims.risk <- function(runs, dose, pop){
    m=DRP$Mean[pop]
    sd= DRP$RefSdLog[pop]
    rlog=rnorm(runs,m,sd)
    r <- 10^rlog
    Pill_cont <- 1 - exp(-r*10^dose)
    return(Pill_cont)
  }
  
  #Predicted probability of illness per contaminated serving
  Pill_cont <- sims.risk(runs=runs, dose=dose, pop=pop)
  
  #Prevalence of contaminated servings - uncertainty. In the baseline here and for the 
  #other RTE foods the mean prevalence is used
  p_beta <- prev$N[]- prev$S[] + 1
  p_alfa <- prev$S[] + 1
  prev_group <- replicate(1000, rbeta(length(p_alfa), p_alfa[], p_beta[]))
  prev_group.means <- rowMeans(prev_group)
  prev_fgroup <- replicate(runs, prev_group.means) #Baseline run - leave in all the time
  
  #Prevalence of contaminated frozen vegetables servings - uncertainty. Edit and run 
  #relevant lines below if uncertainty of the prevalence is to be evaluated 
  #(best, probs=0.025, or worst case, probs=0.975).group 7 is frozen vegetables as RTE,
  #and 15 cooked frozen vegetables. Se input data table 
  
  #Edit (best or worst and group 7 and 15) and run appropriate next four lines to evalaute
  #uncertainty of prevalence
  #scen.prev <- quantile(prev_group[7,],probs=0.025 or 0.975) #Prevalence best case scenario 
  #scen.prev <- quantile(prev_group[15,],probs=0.025 or 0.975) #Prevalence worst case scenario
  
  #Replace prevalence of frozen vegetables with the best or worst case estimate
  #prev_fgroup[7,] <- replicate(runs, scen.prev) #If uncertainty scenario leave
  #prev_fgroup[15,] <- replicate(runs, scen.prev) #If uncertainty scenario leave
  
  #The prevalence of the different food sub categories used in the simulation
  prev_fdgroup <- t(prev_fgroup) 
  
  #Probability of illness per any serving for each foodgroup
  Pill <- Pill_cont * prev_fdgroup
  P_pserv <- data.frame(Pill)
  colnames(P_pserv) <- c("csfR","hsfR","grfR","come","sausR","pateR","fvR","csf","hsf","grf","comeR","saus","pate","sssch","fvco")                     
  
  #P_pserv #Run only if want to inspect results
  
  #Distribution of probability of illness per serving per food group
  perc_quant <- function(x) {
    quantile(x, probs = c(0, 0.025, 0.5, 0.975, 1))
  } 
  
  pserv_mean <- t(sapply(P_pserv, mean))
  pserv_sd <- t(sapply(P_pserv, sd))
  Pserv_res <- as.data.frame(t(sapply(P_pserv, perc_quant)))
  Pserv_res$mean <- as.vector(pserv_mean)
  Pserv_res$sd <- as.vector(pserv_sd)
  
  #Summary of result, probability of illness per serving
  Pserv_res
  rname <- c("csfR","hsfR","grfR","come","sausR","pateR","fvR","csf","hsf","grf","comeR","saus","pate","sssch","fvco")
  
  Pserv_mean <-Pserv_res$mean
  Pill_mean <- data.frame(x=Pserv_res$mean, y=rname)
  colnames(Pill_mean) <- c("mean.Pill.serving", "food.group")
  
  return(list(Pill_mean=Pill_mean[,1]))
}  




# END imported script: m.g.QMRA.R
##############################################





Pill_out <- lapply(1:sims,modelPill_out,
                   pop=pop,runs=runs,
                   prev=prev, conc=conc, EGR=EGR, ROP=ROP, 
                   DRP=DRP, conso=conso, size=size, 
                   r_time=r_time, log_red=log_red)
#Pill_out <- Pill_test$Pill_mean[,1]
#<- lapply(1:sims, function(n)source("~/Projects/ModelRepository/lindqvist_list/originalfiles/m.g.QMRA.R")$value[,1])





#Pill_out #Probability of illness per serving
Pill_df<- data.frame(matrix(unlist(Pill_out), ncol=length(Pill_out), byrow=F))

Pill_df <- data.frame(t(Pill_df))
colnames(Pill_df) <- c("Cold-smoked fish, ROP",
                       "Hot-smoked fish, ROP",
                       "Gravadfish, ROP",
                       "Cooked meat, NAP",
                       "Sausage, ROP",
                       "Pate, ROP",
                       "frozen vegetables, ROP",
                       "Cold-smoked fish, NAP",
                       "Hot-smoked fish, NAP",
                       "Gravad fish, NAP",
                       "Cooked meat, ROP",
                       "Sausage, NAP",
                       "Pate, NAP",
                       "Soft and semi-soft cheese",
                       "frozen vegetables, cooked")
#Pill_df #Data frame with the probability of illness per serving per simulation per food subcategory

#Write the mean probability of illness per serving for the food_category for each simulation 
#write.xlsx(Pill_df, "results.xlsx", sheetName="scenario_name", append=TRUE)

#Description of the outcome of the 250 simulations per food category
perc_quant <- function(x) {
  quantile(x, probs = c(0.025, 0.5, 0.975))
}

Pserv_res <- as.data.frame(sapply(Pill_df[, 1:15], perc_quant)) #15 food subcategories

#Pserv_res

pserv_mean <- sapply(Pill_df[, 1:15], mean)
pserv_sd <- sapply(Pill_df[, 1:15], sd)

Pserv_res <- rbind(Pserv_res, mean=pserv_mean)
Pserv_res <- rbind(Pserv_res, sd=pserv_sd)
#Pserv_res

###Ranking based on the mean probability per serving
pop.group <- DRP[pop,1:2] #population according to indata.file (65-74, females=11, 65-74, males=12)

#Ranking based on mean probability of illness per serving
Pill <- as.data.frame(t(Pserv_res))
Pill_mean_r <- Pill[order(-Pill$mean),]
#Pill_mean_r

#Write rankings of the food subcateregories based on the mean probability of illness per serving 
#write.xlsx(Pill_mean_r, "results2.xlsx", sheetName="scenario_name", append=TRUE)

#############################################
### END model script                      ###
#############################################


#############################
### Visualisation Script  ###
#############################

library(gridExtra)
library(gridGraphics)
library(gtable)

mytheme <- gridExtra::ttheme_default(
  core = list(fg_params=list(cex = 0.76)),
  colhead = list(fg_params=list(cex = 1.0)),
  rowhead = list(fg_params=list(cex = 1.0)))


myt <- gridExtra::tableGrob(signif(Pill_mean_r,digits=3), theme = mytheme)
if (pop==11) {myGroup <- "female"} else {myGroup <- "male"}
myTitle <- paste("Probability of illness per serving\nfor population group:",myGroup)
title <- textGrob(myTitle, gp = gpar(fontsize = 20))
padding <- unit(0.5,"line")
table <- gtable_add_rows(
  myt, heights = grobHeight(title) + padding, pos = 0
)
table <- gtable_add_grob(
  table, list(title),
  t = 1, l = 1, r = ncol(table)
)
frame()
grid.draw(table)
grid.draw(table)
grid.draw(table)
