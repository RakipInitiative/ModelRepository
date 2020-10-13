# set your own path
#setwd("~/BfR/EFSA/gropin")
#setwd("G:/Abteilung-4/Public/FoodRisk-Labs/EFSA-FPA/Area2-SA7/models/gropin")#
#print(getwd())
setwd(dirname(sys.frame(1)$ofile))

listOfNonfunctioningModels <- c(128)

library(readxl)
library(dplyr)
library(gsubfn)

# read in and format certain aspects of database to needs of R
gropinDB <- read_excel("GroPIN-ver.3.xlsm", sheet = "Nonlinear")
names(gropinDB)<-gsub("/","_",names(gropinDB))
# replace "(...string....)" but not in the equation column
gropinDB[,1:94] <- data.frame(lapply(gropinDB[,1:94], 
                                     function(x) {gsub('[()]', '_', x)}))


# filtering models according to identifiers in data base
growthNoGrowthModels <- gropinDB %>% filter(Model == 'GNG')
growthModels <- gropinDB %>% filter(!grepl('INA',INACTIVE)&Model == 'GRT'&!grepl('AUG',AUG_ZU)&!grepl('LTH',LETHALITY))
lethalityModels <- gropinDB %>% filter(grepl('LTH',LETHALITY))
gammaModelsWithInteraction <- gropinDB %>% filter(grepl('AUG',AUG_ZU))
inactivationModels <- gropinDB %>% filter(grepl('INA',INACTIVE))


run <- 22

if(growthModels$Microorganism[run] %in% listOfNonfunctioningModels) {print("Nope")} 


# get all 9 variables from data base
myVarNames <- c(growthModels$x[run],growthModels$y[run],growthModels$z[run],
                growthModels$d[run],growthModels$e[run],growthModels$f[run],
                growthModels$g[run],growthModels$h[run],growthModels$i[run])

if("not used" %in% myVarNames) {
  myVarNames[which(myVarNames %in% "not used")] <- NA
}

# set boundaries of sequence to inner boundaries to catch cases
# where subtraction of variables and coefficents in demoninator happen
myVarMin <- as.double(c(growthModels$from...6[run],growthModels$from...9[run],growthModels$from...12[run],
              growthModels$from...15[run],growthModels$from...18[run],growthModels$from...21[run],
              growthModels$from...24[run],growthModels$from...27[run],growthModels$from...30[run]))*1.001
myVarMax <- as.double(c(growthModels$to...7[run],growthModels$to...10[run],growthModels$to...13[run],
              growthModels$to...16[run],growthModels$to...19[run],growthModels$to...22[run],
              growthModels$to...25[run],growthModels$to...28[run],growthModels$to...31[run]))*0.999


# different models have different number of variables (rest is NA)
nrOfVariables <- length(table(myVarNames))
mymy <- rep(NA,nrOfVariables)

# create the R script text file with only regular variables
for (j in 1:nrOfVariables) {
  mymy[j] <- paste0(myVarNames[j], 
                    " <- seq(" , myVarMin[j], ",",myVarMax[j],
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
if (nrOfVariables<2) {
  print("TODO!")#nothing to choose, but different plot
} 
if (nrOfVariables>2) {
  # choose visualisation layer
  mymy <- append(mymy,paste0("visVar1 <- '",myVarNames[1],
                             "'\nvisVar2 <- '",myVarNames[2],"'"))
}


# comments to identify for parameter script
mymy <- append(mymy,"#############################\n# start of Parameter script\n#############################", after=0)
mymy <- append(mymy,"#############################\n# end of Parameter script\n#############################")

mymy <- append(mymy," ")
# comments to identify for model script
mymy <- append(mymy,"#############################\n# start of Model script\n#############################")
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
                               ") {",myVarNames[varrun]," <- 0}"))
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
  otherString <- "notVisibleAxes[1],notVisibleAxes[2]"
  mymy <- append(mymy,paste0("result <- outer(multVar1,multVar2,response_surface,",
                             otherString,
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
mymy <- append(mymy,"#############################\n# End of Model script\n#############################")
mymy <- append(mymy," ")
# comments to identify for Visualisation script
mymy <- append(mymy,"#############################\n# start of Visualisation script\n#############################")
if (nrOfVariables>2) {
  mymy <- append(mymy,"persp(multVar1,multVar2,result,col = 'green',xlab=visVar1,ylab=visVar2,zlab='mu_max',theta=35,phi=20,shade=0.25,ticktype = 'detailed')")
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


# store to disk
fileConn<-file("output.r")
writeLines(mymy, fileConn)
close(fileConn)

