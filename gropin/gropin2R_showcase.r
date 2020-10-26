# set your own path
#setwd("~/BfR/EFSA/gropin")
#setwd("G:/Abteilung-4/Public/FoodRisk-Labs/EFSA-FPA/Area2-SA7/models/gropin")#
#print(getwd())
listOfNonfunctioningModels <- c(128,256)
run <- 348
innerBound <- F
showCase <- 'responsesurface'
#showCase <- 'time2multiply'
#showCase <- 'kinetic'



# path of running script in same folder as datasets
setwd(dirname(sys.frame(1)$ofile))

library(readxl)
library(writexl)
library(dplyr)
library(gsubfn)

# read in and format certain aspects of database to needs of R
gropinDB <- read_excel("GroPIN-ver.3.xlsm", sheet = "Nonlinear")
names(gropinDB)<-gsub("/","_",names(gropinDB))
# replace "(...string....)" but not in the equation column
gropinDB[,1:94] <- data.frame(lapply(gropinDB[,1:94], 
                                     function(x) {gsub('[()]', '', x)}))


# filtering models according to identifiers in data base
growthNoGrowthModels <- gropinDB %>% filter(Model == 'GNG')
growthModels <- gropinDB %>% filter(!grepl('INA',INACTIVE)&Model == 'GRT'&!grepl('AUG',AUG_ZU)&!grepl('LTH',LETHALITY))
lethalityModels <- gropinDB %>% filter(grepl('LTH',LETHALITY))
gammaModelsWithInteraction <- gropinDB %>% filter(grepl('AUG',AUG_ZU))
inactivationModels <- gropinDB %>% filter(grepl('INA',INACTIVE))



if(growthModels$Microorganism[run] %in% listOfNonfunctioningModels) {print("Nope")} 


# get all 9 variables from data base
myVarNames <- c(growthModels$x[run],growthModels$y[run],growthModels$z[run],
                growthModels$d[run],growthModels$e[run],growthModels$f[run],
                growthModels$g[run],growthModels$h[run],growthModels$i[run])

if("not used" %in% myVarNames) {
  myVarNames[which(myVarNames %in% "not used")] <- NA
}


# different models have different number of variables (rest is NA)
nrOfVariables <- length(table(myVarNames))
mymy <- rep(NA,nrOfVariables)
myVal <- rep(NA,nrOfVariables)


myVarMin <- as.double(c(growthModels$from...6[run],growthModels$from...9[run],growthModels$from...12[run],
              growthModels$from...15[run],growthModels$from...18[run],growthModels$from...21[run],
              growthModels$from...24[run],growthModels$from...27[run],growthModels$from...30[run]))
myVarMax <- as.double(c(growthModels$to...7[run],growthModels$to...10[run],growthModels$to...13[run],
              growthModels$to...16[run],growthModels$to...19[run],growthModels$to...22[run],
              growthModels$to...25[run],growthModels$to...28[run],growthModels$to...31[run]))

maxMult <- 1
minMult <- 1

# set boundaries of sequence to inner boundaries to catch cases
# where subtraction of variables and coefficents in demoninator happen AND coincide
if(innerBound) {
  maxMult <- 0.999
  minMult <- 1.001
}

# for cases where min & max was put in wrong :(
# TODO: vectorize!
for(myVarOrder in 1:nrOfVariables){
  if(myVarMin[myVarOrder]>myVarMax[myVarOrder]){
    help <- myVarMax[myVarOrder]
    myVarMax[myVarOrder] <- myVarMin[myVarOrder]
    myVarMin[myVarOrder] <- help
  }
  myVarMax[myVarOrder] <- myVarMax[myVarOrder]*maxMult
  myVarMin[myVarOrder] <- myVarMin[myVarOrder]*minMult
}



# create the R script text file with only regular variables
for (j in 1:nrOfVariables) {
  mymy[j] <- paste0(myVarNames[j],
                    " <- seq(" , 
                    myVarMin[j], 
                    ",",
                    myVarMax[j],
                    ",length.out=21)")
  myVal[j] <- paste0("seq(" , 
                     myVarMin[j], 
                     ",",
                     myVarMax[j],
                     ",length.out=21)")
  
}


# different models have different coefficients 
namesOfCoeffsList<-growthModels[run,names(growthModels)[53:92][c(TRUE,FALSE)]]
valuesOfCoeffsList<-growthModels[run,names(growthModels)[53:92][c(FALSE,TRUE)]]

if (namesOfCoeffsList[1]!='not used') {
  namesOfCoeffs <- as.character(namesOfCoeffsList)[!is.na(as.character(namesOfCoeffsList))]
  valuesOfCoeffs <- as.character(valuesOfCoeffsList)[!is.na(as.character(valuesOfCoeffsList))]
  nrOfCoeffs <- length(namesOfCoeffs)
  
  for (c in 1:nrOfCoeffs) {
    mymy <- append(mymy,paste0(namesOfCoeffs[c]," <- ",valuesOfCoeffs[c]))
  }
  
}


# choosing axes to visualize
if (nrOfVariables>2) {
  # choose visualisation layer
  mymy <- append(mymy,paste0("visVar1 <- '",myVarNames[1],
                             "'\nvisVar2 <- '",myVarNames[2],"'"))
}
mymy <- append(mymy,paste0("mode <-'",showCase,"'"))
#if (showCase == 'responsesurface') {}

#if (showCase == 'time2multiply') {}
mymy <- append(mymy,"lagTime <- 10.0")
mymy <- append(mymy,"logIncrease <- 1.0")

#if (showCase == 'kinetic') {}
mymy <- append(mymy,"logN0 <- 0.0")
mymy <- append(mymy,"logNEnd <- 10.0")
mymy <- append(mymy,"simTime <- 100.0")

for(kinPar in 1:nrOfVariables){
  mymy <- append(mymy,paste0(myVarNames[kinPar],"_kinetic <- ",(myVarMax[kinPar]+myVarMin[kinPar])/2))
}

# comments to identify for parameter script
mymy <- append(mymy,"#############################\n# start of Parameter script\n#############################", after=0)
mymy <- append(mymy,"#############################\n# end of Parameter script\n#############################")

myParScript <- mymy
mymy <- append(mymy," ")
# comments to identify for model script
mymy <- "#############################\n# start of Model script\n#############################"
if(nrOfVariables>2) {
  mymy <- append(mymy,"library(hash)")
  myString<-replicate(2,myVarNames[1:nrOfVariables])
  myString<-paste(myString[,1], myString[,2], sep = "=", collapse = ",")
  mymy <- append(mymy,paste0("myHash <- hash(",myString,")"))
#  mymy <- append(mymy,paste0("expectedAxes <- c('",
#                             paste(myVarNames[1:nrOfVariables],collapse = '\',\''),"')"))
#  mymy <- append(mymy,"myString <- replicate(2,expectedAxes)"
  
}
#mymy <- append(mymy,paste0("myDict <- hash(",
 #                          t,
  #                         ")"))



# as for the equation for the response surface model
gropinVarNames <- c("B2","C2","D2",
                    "E2","F2","G2",
                    "H2","I2","J2")
gropinCoeffNames <- c("K2","M2","O2","Q2","S2",
                      "U2","W2","Y2","AA2","AC2",
                      "AE2","AG2","AH2","AJ2","AL2",
                      "AN2","AP2","AR2","AT2","AV2")
gropinFunctionNames <- c("SQRT")
FunctionNames <- c("sqrt")

# replacing gropin variable names with correct names
myEq <- gsubfn("\\w+",as.list(setNames(myVarNames[1:nrOfVariables],
                                       gropinVarNames[1:nrOfVariables])),
               growthModels$equation[run])

myEq <- gsubfn("\\w+",as.list(setNames(FunctionNames,
                                       gropinFunctionNames)),
               myEq)


# adding coefficients to parameter list
if (namesOfCoeffsList[1]!='not used') {
  #replacing gropin coeff names with correct names
  myEq <- gsubfn("\\w+",as.list(setNames(namesOfCoeffs,
                                         gropinCoeffNames[1:length(namesOfCoeffs)])),
                 myEq)
}

# adding conditional visualisation parameters to model code 
# necessary to run here, because needed for equation to run with chosen visPars 
# as sequence
if (nrOfVariables>2) {
  for (varrun in 1:nrOfVariables) {
    mymy <- append(mymy,paste0("if (visVar1 == '",myVarNames[varrun],
                               "') {\n  multVar1 <- ",myVarNames[varrun],"\n}"))
  }
  mymy <- append(mymy," ")
  for (varrun in 1:nrOfVariables) {
    mymy <- append(mymy,paste0("if (visVar2 == '",myVarNames[varrun],
                               "') {\n  multVar2 <- ",myVarNames[varrun],"\n}"))
  }
  mymy <- append(mymy,"visAxes <- c(visVar1,visVar2)")
  mymy <- append(mymy,paste0("expectedAxes <- c('",
                             paste(myVarNames[1:nrOfVariables],collapse = '\',\''),"')"))
  mymy <- append(mymy,"notPresent<-match(expectedAxes,visAxes)")
  for (varrun in 1:nrOfVariables) {
    mymy <- append(mymy,paste0("if('",
                               myVarNames[varrun],
                               "' %in% expectedAxes[is.na(notPresent)]",
                               ") {myHash['",myVarNames[varrun],"'] <- 0}"))
  }
}
if (nrOfVariables==2) {
  for (varrun in 1:nrOfVariables) {
    mymy <- append(mymy,paste0("multVar",varrun," <- ",myVarNames[varrun]))
  }
}
  
mymy <- append(mymy," ")
# here is the equation to calculate mumax added to the script
mymy <- append(mymy,paste0("response_surface <- function(",
                           paste(myVarNames[1:nrOfVariables],collapse = ','),") {\n   ",myEq,"\n} "))
restVariables <- nrOfVariables-2
if (nrOfVariables>2) {
  mymy <- append(mymy," ")
  mymy <- append(mymy,"notVisibleAxes <- expectedAxes[!expectedAxes %in% visAxes]")
  mymy <- append(mymy,paste0("result <- outer(as.double(values(myHash[visVar1])),as.double(values(myHash[visVar2])),response_surface,",
                             paste0(replicate(restVariables,"as.double(values(myHash[notVisibleAxes["),1:restVariables,"]]))",collapse = ","),
                 ")"))
}
if (nrOfVariables==2) {
  mymy <- append(mymy,
                 paste0("result <- outer(multVar1,multVar2,response_surface)"))
}
if (nrOfVariables==1) {
  mymy <- append(mymy,
                 paste0("result <- response_surface(",
                        myVarNames[1],
                        ")"))
  }
if (nrOfVariables>1){
  mymy <- append(mymy,"colnames(result)<-multVar2")
  mymy <- append(mymy,"rownames(result)<-multVar1")
}

#if (showCase == 'time2multiply') {}
#mymy <- append(mymy,"lagTime <- 10.0")
#mymy <- append(mymy,"logIncrease <- 1.0")
mymy <- append(mymy,"if(mode=='time2multiply') {\n  time2Xlog <- lagTime + logIncrease/result\n}")

#if (showCase == 'kinetic') {}

mymy <- append(mymy,paste0("if(mode=='kinetic') {
  time2Xlog <- lagTime + logIncrease/result
  q0 <- 1/(exp(lagTime)-1)
  mumax <- response_surface(",paste(myVarNames[1:nrOfVariables],collapse='_kinetic,'),"_kinetic)"))
mymy <- append(mymy,"  t <- seq(0,simTime,length.out = 71)
  A <- t + (1/mumax)*log((exp(-mumax*t)+q0)/(1+q0))
  logN = logN0 + mumax*A - log(1+((exp(mumax*A)-1)/(exp(logNEnd-logN0))))
}")



mymy <- append(mymy,"#############################\n# End of Model script\n#############################")
myModelScript <- mymy
#mymy <- append(mymy," ")
# comments to identify for Visualisation script
mymy <- "#############################\n# start of Visualisation script\n#############################"
if (nrOfVariables>2) {
  mymy <- append(mymy,paste("if(mode=='responsesurface') {\n  persp(as.double(values(myHash[visVar1])),
                 as.double(values(myHash[visVar2])),
                 result,
                 col = 'green',
                 xlab=keys(myHash[visVar1]),
                 ylab=keys(myHash[visVar2]),
                 zlab='mu_max',
                 main='response surface mu_max for Gropin Model Nr.",growthModels$Microorganism[run],"',
                 theta=35,
                 phi=20,
                 shade=0.25,
                 ticktype = 'detailed')\n}"))
  mymy <- append(mymy,paste("if(mode=='time2multiply') {
                 \n  myZ <- paste('time to increase',logIncrease,'step(s)')
                 \n  persp(as.double(values(myHash[visVar1])),
                 as.double(values(myHash[visVar2])),
                 time2Xlog,
                 col = 'green',
                 xlab=keys(myHash[visVar1]),
                 ylab=keys(myHash[visVar2]),
                 zlab=myZ,
                 main='Time in h to increase log step for Gropin Model Nr.",growthModels$Microorganism[run],"',
                 theta=35,phi=20,shade=0.25,ticktype = 'detailed')\n}"))
  mymy <- append(mymy,paste("if(mode=='kinetic') {
               \n  plot(t,logN,
               xlab='t in h',
               ylab='log N in CFU/g',
               main='Gropin Model Nr.",growthModels$Microorganism[run],"')
               \n}"))
}
if (nrOfVariables==2) {
  mymy <- append(mymy,paste0("persp(multVar1,multVar2,result,col = 'green',xlab='",
  myVarNames[1],
  "',ylab='",
  myVarNames[2],
  "',zlab='mu_max',theta=35,phi=20,shade=0.25,ticktype = 'detailed')"))
}
if (nrOfVariables==1){
  mymy <- append(mymy,
                 paste0("plot(",
                        myVarNames[1],
                        ",result,xlab='",
                        myVarNames[1],
                        "',ylab='mu_max')"))
}
mymy <- append(mymy,"#############################\n# End of Visualisation script\n#############################")
myVisScript <- mymy

# store to disk
fileConn<-file("showcase/showcase_par.r")
writeLines(myParScript, fileConn)
close(fileConn)


fileConn<-file("showcase/show_case_mod.r")
writeLines(myModelScript, fileConn)
close(fileConn)

fileConn<-file("showcase/showcase_vis.r")
writeLines(myVisScript, fileConn)
close(fileConn)


###################################################################
# editing Annotation spreadsheet
##################################################################
MetaData <- read_excel("ModelAnnotationExceltemplateV1.04.xlsx", sheet = "Generic Metadata Schema")

# mandatory fields
#Name of the Model
MetaData$Data[1] <-paste("Gropin growth model for",growthModels$M_O[run],"in/on",growthModels$Product[run],"(gropin ID:",growthModels$Microorganism[run],")")
MetaData$Data[2] <- "PUBLISHED SCIENTIFIC STUDIES"
#Identifier
MetaData$Data[3] <- paste0("gropin",growthModels$Model[run],growthModels$Microorganism[run])
MetaData$Data[6] <- "22-10-2020"
MetaData$Data[8] <- "Value"
MetaData$Data[9] <- "Open access" 
MetaData$Data[10] <- "https://www.aua.gr/psomas/gropin/"
MetaData$Data[11] <- ".fskx" 
MetaData$Data[24] <- "English" 
MetaData$Data[25] <- "R" 
MetaData$Data[26] <- "R 3"
MetaData$Data[32] <- "Curated"
MetaData$Data[33] <- paste0("The objective is to model the microorganism ",
                            growthModels$M_O[run],
                            " with the substrate ",
                            growthModels$Substrate[run],
                            " for the product ",
                            growthModels$Product[run],
                            ", by calculating ",
                            growthModels$`Rate label`[run],
                            " with the unit ",
                            growthModels$mumaxUn[run],".")
MetaData$Data[34] <- paste0("Special Notes from Model: ",
                            growthModels$`Special notes`[run])



# model category
MetaData$Data[27] <- "QRA model"
MetaData$Data[28] <- "growth model"
MetaData$Data[29] <- "This model has 3 modi: response surface mode, time 2 multiply mode and kinetic mode"

# creator
MetaData$...13[3] <- "Marcel" #given
MetaData$...15[3] <- "Fuhrmann" #family
MetaData$...18[3] <- "marcel.fuhrmann@bfr.bund.de" #email
MetaData$...16[3] <- "BfR" #organization


# author 
# TODO separate authors for dynamic writing! 
# id number of authors!
nrOfAuthors <- 4 
authorVec <- unlist(strsplit(growthModels$Authors[run],","))
for(a in 1:nrOfAuthors) {
  currentRow <- a+2
  MetaData$...29[currentRow] <- authorVec[2*a] #given
  MetaData$...31[currentRow] <- authorVec[2*a-1] #family
  MetaData$...34[currentRow] <- " " #email
}


#reference
MetaData$Creator[14] <- "Yes"
MetaData$...12[14] <- "Journal Article" 
MetaData$...13[14] <- authorVec[length(authorVec)]
MetaData$...15[14] <- " " 
MetaData$...16[14] <- paste(authorVec[1:length(authorVec)-1],collapse=",")
MetaData$...17[14] <- growthModels$Paper[run]
MetaData$...19[14] <- paste("Journal",growthModels$...34[run],"Issue",growthModels$Issue[run]) 
MetaData$...20[14] <- "Published" 


#product
MetaData$Creator[38] <- growthModels$Product[run] #name
MetaData$...13[38] <- "CFU/g" #unit

#hazard
MetaData$...22[38] <- "Microorganisms"#type
MetaData$...23[38] <- growthModels$M_O[run]#name
MetaData$...24[38] <- " "#description
  
# parameters
firstRow <- 132
thisRow <- firstRow
for(fskPar in 1:nrOfVariables){
  MetaData$...12[thisRow] <- myVarNames[fskPar]
  MetaData$...13[thisRow] <- "Input"
  MetaData$...14[thisRow] <- myVarNames[fskPar]
  MetaData$...15[thisRow] <- "Only applicaple in either mode 'responsesurface' or 'time2multiply'. Environmental factors for growth model."#"my dummy description"
  MetaData$...16[thisRow] <- "[]"
  MetaData$...17[thisRow] <- ""#unit
  MetaData$...18[thisRow] <- "Vector[number]"
  MetaData$...19[thisRow] <- ""#"my source"
  MetaData$...20[thisRow] <- ""#"my subject"
  MetaData$...21[thisRow] <- ""#"mydist"
  MetaData$...22[thisRow] <- myVal[fskPar]
  thisRow <- thisRow+1
}

if (namesOfCoeffsList[1]!='not used') {
  for(fskCoeff in 1:nrOfCoeffs){
    MetaData$...12[thisRow] <- namesOfCoeffs[fskCoeff]
    MetaData$...13[thisRow] <- "Input"
    MetaData$...14[thisRow] <- namesOfCoeffs[fskCoeff]
    MetaData$...15[thisRow] <- "coefficient of this particular model"#"my dummy description"
    MetaData$...16[thisRow] <- "[]"
    MetaData$...17[thisRow] <- ""#unit
    MetaData$...18[thisRow] <- "Double"
    MetaData$...19[thisRow] <- ""#"my source"
    MetaData$...20[thisRow] <- ""#"my subject"
    MetaData$...21[thisRow] <- ""#"mydist"
    MetaData$...22[thisRow] <- valuesOfCoeffs[fskCoeff]
    thisRow <- thisRow+1
  }
}

# inserting all other parameters
namesOfAddPars <- c("visVar1",
                    "visVar2",
                    "mode",
                    "lagTime",
                    "logIncrease",
                    "logN0",
                    "logNEnd",
                    "simTime")
for(addPar in 1:nrOfVariables) {
  namesOfAddPars <- append(namesOfAddPars,paste0(myVarNames[addPar],"_kinetic"))
}
#myPar <- namesOfAddPars[(length(namesOfAddPars)-nrOfVariables+1):length(namesOfAddPars)]

valuesOfAddPars <- c(paste0("'",myVarNames[1],"'"),
                     paste0("'",myVarNames[2],"'"),
                     paste0("'","responsesurface","'"),
                    "10",
                    "1",
                    "0",
                    "10",
                    "1000")
descrOfAddPars <- c(paste("For visualisation purposes in either mode 'time2multiply' or 'responsesurface'. visualisation axis. Enter string with '<variable ID>'. Strings that are accepted:",paste(myVarNames[!is.na(myVarNames)],collapse = ", ")),
                    paste("For visualisation purposes in either mode 'time2multiply' or 'responsesurface'. visualisation axis. Enter string with '<variable ID>'. Strings that are accepted:",paste(myVarNames[!is.na(myVarNames)],collapse = ", ")),
                    "three different modes are available: 'responsesurface' is running the secondary model calculating mumax only. 'time2multiply' returns a 2D-plot of the time the microorganism needs to increase N by a logstep of 'logIncrease'(free parameter to choose). 'kinetic' runs the tertiary model, based on the variables chosen (with '_kinetic'-suffix).",
                    "Only applicaple in either mode 'time2multiply' or 'kinetic' is chosen. This is the time the microorganism needs for adjusting to its environment before multiplying.",
                    "Only applicaple in mode 'time2multiply'. Free parameter to calculate the time the microorganism needs to increase its numbers by the log step increase indictated by this value.",
                    "Only applicaple in either mode 'kinetic'. Choose the number of microorganisms at the beginning of this simulation. (log step!)",
                    "Only applicaple in either mode 'kinetic'. Choose the number of microorganisms at the end of this simulation. (log step!)",
                    paste("time of simulation, unit is",growthModels$mumaxUn[run]))

for(addVal in 1:nrOfVariables) {
  valuesOfAddPars <- append(valuesOfAddPars,(myVarMax[addVal]+myVarMin[addVal])/2)
  descrOfAddPars <- append(descrOfAddPars,"Only applicaple in either mode 'kinetic'. Choose variable for prediction of growth depending on environmental factors.")
}


for(morePars in 1:length(namesOfAddPars)){
  MetaData$...12[thisRow] <- namesOfAddPars[morePars]
  MetaData$...13[thisRow] <- "Input"
  MetaData$...14[thisRow] <- namesOfAddPars[morePars]
  MetaData$...15[thisRow] <- descrOfAddPars[morePars]
  MetaData$...16[thisRow] <- "[]"
  MetaData$...17[thisRow] <- ""#unit
  if(morePars<4){
    MetaData$...18[thisRow] <- "String"
  } else {
    MetaData$...18[thisRow] <- "Double"
  }
  MetaData$...19[thisRow] <- ""#"my source"
  MetaData$...20[thisRow] <- ""#"my subject"
  MetaData$...21[thisRow] <- ""#"mydist"
  MetaData$...22[thisRow] <- valuesOfAddPars[morePars]
  thisRow <- thisRow+1
  
}

# getting all other information
#growthModels$Substrate #substrate ->?
#growthModels$Authors #authors
#growthModels$Paper #authors
#growthModels$...34 #Journal
#growthModels$Issue #issue
#growthModels$M_O #name of microorganism -> hazard
#growthModels$Product #product
#growthModels$Authors #authors
#growthModels$mumaxUn #mumax unit ->?
#growthModels$`Rate label` # name of mumax ->?
#growthModels$`Special notes`[run] # info to find equation in paper -> comment?
#growthModels$`Reference equation`[run] #units for variables? ->?


write_xlsx(list("Generic Metadata Schema"=MetaData),
           path="showcase/showcase_MD.xlsx")
