################################################################################
# setting a few parameters for potential later adaptations to gropin
################################################################################
lenOfVar <- 21

################################################################################






# set your own path
setwd("~/Projects/ModelRepository/gropin")
#setwd(dirname(sys.frame(1)$ofile)) # current working directory(only works in source mode)

# finding weird models with lots of exceptions
# thant one wants to exclude until further notice
listOfNonfunctioningModels <- NA

library(readxl)
library(dplyr)
library(gsubfn)
library(writexl)
################################################################################
# START PART ONE: Preprocessing 
################################################################################

#renaming table header for transparency purposes
newNamesForGropinColumns <- c("Model",
                              "ModelID",
                              "Var",
                              "Substrate",
                              "Var1",
                              "Var1Min",
                              "Var1Max",
                              "Var2",
                              "Var2Min",
                              "Var2Max",
                              "Var3",
                              "Var3Min",
                              "Var3Max",
                              "Var4",
                              "Var4Min",
                              "Var4Max",
                              "Var5",
                              "Var5Min",
                              "Var5Max",
                              "Var6",
                              "Var6Min",                 
                              "Var6Max",
                              "Var7",
                              "Var7Min",                 
                              "Var7Max",
                              "Var8",
                              "Var8Min",                 
                              "Var8Max",
                              "Var9",
                              "Var9Min",                 
                              "Var9Max",
                              "Authors",    
                              "Paper",        
                              "Journal",        
                              "Issue",                 
                              "Var10",                         
                              "Var10Min",           
                              "Var10Max",             
                              "INACTIVE",                  
                              "Microorganism",          
                              "First author",      
                              "Product",                   
                              "psicheck",         
                              "LETHALITY",           
                              "DMRI",                      
                              "MODELCATEG",           
                              "INTEGRATED",           
                              "mumaxUn",                   
                              "AUG_ZU",              
                              "Rate label",            
                              "Special notes",             
                              "Reference equation",  
                              "Co1",              
                              "Co1val",                    
                              "Co2",               
                              "Co2val",            
                              "Co3",                       
                              "Co3val",            
                              "Co4",              
                              "Co4val",                    
                              "Co5",             
                              "Co5val",                  
                              "Co6",                       
                              "Co6val", 
                              "Co7",      
                              "Co7val", 
                              "Co8",     
                              "Co8val", 
                              "Co9",       
                              "Co9val", 
                              "Co10",    
                              "Co10val",
                              "Co11",   
                              "Co11val", 
                              "Co12",     
                              "Co12val",          
                              "Co13",           
                              "Co13val",                   
                              "Co14",                 
                              "Co14val",          
                              "Co15",                      
                              "Co15val",              
                              "Co16",             
                              "Co16val",                   
                              "Co17",                 
                              "Co17val",           
                              "Co18",                      
                              "Co18val",              
                              "Co19",              
                              "Co19val",                   
                              "Co20", 
                              "Co20val",                
                              "Type of simulation",        
                              "mumax",                 
                              "equation")


# read in and format certain aspects of database to needs of R
#gropinDB <- read_excel("GroPIN-ver.3.xlsm", sheet = "Nonlinear") # 1st version we did try to convert
gropinDB <- read_excel("GroPIN201031.xlsm", sheet = "Nonlinear") # current version

# replace some special characters
names(gropinDB)<-gsub("/","_",names(gropinDB))
# replace "(...string....)" but not in the equation column (column 95 aka last)
gropinDB[,1:94] <- data.frame(lapply(gropinDB[,1:94], 
                                     function(x) {gsub('[()]', '_', x)}))


names(gropinDB) <- newNamesForGropinColumns
# filtering models according to identifiers in data base
# currently, only growthModels are considered.
# future versions of this transfer code will include all types of models
growthNoGrowthModels <- gropinDB %>% filter(Model == 'GNG')
growthModels <- gropinDB %>% filter(!grepl('INA',INACTIVE)&
                                      Model == 'GRT'&
                                      !grepl('AUG',AUG_ZU)&
                                      !grepl('LTH',LETHALITY))
lethalityModels <- gropinDB %>% filter(grepl('LTH',LETHALITY))
gammaModelsWithInteraction <- gropinDB %>% filter(grepl('AUG',AUG_ZU))
inactivationModels <- gropinDB %>% filter(grepl('INA',INACTIVE))

################################################################################
# END PART ONE: Preprocessing 
################################################################################
run <-32


################################################################################
# START PART TWO: creating scripts
################################################################################
nrModels <- dim(growthModels)[1]

#for(run in 1:nrModels){
  
  # exlude model from conversion any model given in list above
  if(growthModels$ModelID[run] %in% listOfNonfunctioningModels) {
    print("Nope")
    next
  } 
  
  # get all 9 variables from data base
  # variable has the following properties: name, range(from, to)
  # no unit is given, no description
  myVarNames <- c(as.character(growthModels$Var1[run]),
                  as.character(growthModels$Var2[run]),
                  as.character(growthModels$Var3[run]),
                  as.character(growthModels$Var4[run]),
                  as.character(growthModels$Var5[run]),
                  as.character(growthModels$Var6[run]),
                  as.character(growthModels$Var7[run]),
                  as.character(growthModels$Var8[run]),
                  as.character(growthModels$Var9[run]),
                  as.character(growthModels$Var10[run]))
  # some names of parameters have special characters, 
  # that need to be dealt with -> names of variables will become variable names 
  # in their own R scripts, therefore must be free of special characters
  myVarNames <- gsub("[+]","",myVarNames)
  myVarNames <- gsub(" ","",myVarNames)
  
  # phrase of gropin, marks end of list of variables
  if("not used" %in% myVarNames) {
    myVarNames[which(myVarNames %in% "not used")] <- NA
  }
  
  
  # different models have different number of variables (rest is NA)
  nrOfVariables <- length(table(myVarNames))
 
  
  # set boundaries of sequence to inner boundaries to catch cases
  # where subtraction of variables and coefficents in demoninator happen
  myVarMin <- c(as.double(as.character(growthModels$Var1Min[run])),
                as.double(as.character(growthModels$Var2Min[run])),
                as.double(as.character(growthModels$Var3Min[run])),
                as.double(as.character(growthModels$Var4Min[run])),
                as.double(as.character(growthModels$Var5Min[run])),
                as.double(as.character(growthModels$Var6Min[run])),
                as.double(as.character(growthModels$Var7Min[run])),
                as.double(as.character(growthModels$Var8Min[run])),
                as.double(as.character(growthModels$Var9Min[run])),
                as.double(as.character(growthModels$Var10Min[run])))
  myVarMax <- c(as.double(as.character(growthModels$Var1Max[run])),
                as.double(as.character(growthModels$Var2Max[run])),
                as.double(as.character(growthModels$Var3Max[run])),
                as.double(as.character(growthModels$Var4Max[run])),
                as.double(as.character(growthModels$Var5Max[run])),
                as.double(as.character(growthModels$Var6Max[run])),
                as.double(as.character(growthModels$Var7Max[run])),
                as.double(as.character(growthModels$Var8Max[run])),
                as.double(as.character(growthModels$Var9Max[run])),
                as.double(as.character(growthModels$Var10Max[run])))
  
  # for cases where min & max was put in wrong in gropin
  for(myVarOrder in 1:nrOfVariables){
    if(myVarMin[myVarOrder]>myVarMax[myVarOrder]){
      help <- myVarMax[myVarOrder]
      myVarMax[myVarOrder] <- myVarMin[myVarOrder]
      myVarMin[myVarOrder] <- help
    }
  }
  
  
  ###########################################################
  # create the R script text file with only regular variables
  ###########################################################
  # setting parameters: variables (1st step defining r script)
  # later converting this script to fskx
  # note that parameter script is not used for transfer to fskx
  # rather it is used to test if this would work as an r-script
  # for bug fixing purposes
  ###########################################################
  # comments to identify for parameter script
  myParScript <- "#############################\n# start of Parameter script\n#############################"
  valuesForParameters <- rep(NA,nrOfVariables)
  #adding all variables to parameter script
   for (j in 1:nrOfVariables) {
    myParScript <- append(myParScript,paste0(myVarNames[j], 
                      " <- seq(" , 
                      myVarMin[j], 
                      ",",
                      myVarMax[j],
                      ",length.out=",
                      lenOfVar,
                      ")")
    )
    
    # create vector of input values for annotation schema later
    valuesForParameters[j] <- paste0("seq(" , 
                                  myVarMin[j], 
                                  ",",
                                  myVarMax[j],
                                  ",length.out=",
                                  lenOfVar,
                                  ")")
    
   }
  
  
  
  # different models have different coefficients 
  namesOfCoeffsList<-growthModels[run,names(growthModels)[53:92][c(TRUE,FALSE)]]
  valuesOfCoeffsList<-growthModels[run,names(growthModels)[53:92][c(FALSE,TRUE)]]
  
  # extracting actual names and values only and remove potential special characters
  if (namesOfCoeffsList[1]!='not used') {
    namesOfCoeffs <- as.character(unlist(namesOfCoeffsList))[!is.na(as.character(unlist(namesOfCoeffsList)))]
    namesOfCoeffs <- gsub("[+]","",namesOfCoeffs)
    namesOfCoeffs <- gsub(" ","",namesOfCoeffs)
    valuesOfCoeffs <- as.character(unlist(valuesOfCoeffsList))[!is.na(as.character(valuesOfCoeffsList))]
    nrOfCoeffs <- length(namesOfCoeffs)
    
    for (c in 1:nrOfCoeffs) {
      myParScript <- append(myParScript,paste0(namesOfCoeffs[c]," <- ",valuesOfCoeffs[c]))
    }
    
  }
  
  
  # choosing axes to visualize
  if (nrOfVariables>2) {
    # choose visualisation layer
    myParScript <- append(myParScript,paste0("visVar1 <- '",myVarNames[1],
                               "'\nvisVar2 <- '",myVarNames[2],"'"))
  }
  myParScript <- append(myParScript,"#############################\n# end of Parameter script\n#############################")
  
  ##############################################################################
  # end of setting parameters
  ##############################################################################
  
  
  ##############################################################################
  # Start model script
  ##############################################################################
  
  # comments to identify for model script
  myModelScript <- "#############################\n# start of Model script\n#############################"

  # column "equation" is evaluated
  # as for the equation for the response surface model
  # in gropin excel code, variables are referred to their position in the 
  # corresponding sheet -> in "equation" positions are converted into 
  # variable names
  gropinVarNames <- c("B2","C2","D2",
                      "E2","F2","G2",
                      "H2","I2","J2")
  gropinCoeffNames <- c("K2","M2","O2","Q2","S2",
                        "U2","W2","Y2","AA2","AC2",
                        "AE2","AG2","AH2","AJ2","AL2",
                        "AN2","AP2","AR2","AT2","AV2")

  # replacing gropin excel position names with actual variable names
  myEq <- gsubfn("\\w+",as.list(setNames(myVarNames[1:nrOfVariables],
                                         gropinVarNames[1:nrOfVariables])),
                 growthModels$equation[run])

  # gropin uses excel functions to make certain calculations
  # in order to convert each line to R, these function names
  # need to be corrected
  # SOLUTION: collect the names of excel functions and
  # the names of R functions in a list
  gropinFunctionNames <- c("SQRT")
  FunctionNames <- c("sqrt")
  
  # replacing function names
  myEq <- gsubfn("\\w+",as.list(setNames(FunctionNames,
                                         gropinFunctionNames)),
                 myEq)
  
  
  # adding coefficients to parameter list
  if (namesOfCoeffsList[1]!='not used') {
    #replacing gropin coeff names with correct names
    myEq <- gsubfn("\\w+",
                   as.list(setNames(namesOfCoeffs,
                                           gropinCoeffNames[1:length(namesOfCoeffs)])),
                   myEq)
  }
  

  myModelScript <- append(myModelScript," ")
  myModelScript <- append(myModelScript,
                          paste0("variables <- data.frame(",
                                 paste(myVarNames[1:nrOfVariables],collapse = ','),
                                 ")")
  )
  myModelScript <- append(myModelScript,
                          "argumentsPar <- expand.grid(variables)"
  )
  
  
  # here is the equation to calculate mumax added to the script
  myModelScript <- append(myModelScript,
                          paste0("response_surface <- function(",
                                 paste(myVarNames[1:nrOfVariables],collapse = ','),
                             ") {\n   mumax <-matrix(unlist(",
                             myEq,
                             ",nrow=",
                             lenOfVar,
                             "))\nreturn(mumax=mumax)\n} "))

  parametersForFunctionCall <- paste0()
  myModelScript <- append(myModelScript, 
                          paste0("mumax <- response_surface(",
                                 paste0("argumentsPar['",myVarNames[1:nrOfVariables],"']",collapse = ','),
                          ")")
                          )
  myModelScript <- append(myModelScript,"#############################\n# End of Model script\n#############################")
  
  ##############################################################################
  # End of Model Scripting
  ##############################################################################
  
  ##############################################################################
  # Start of Visualisation Scripting
  ##############################################################################
  
  # comments to identify for Visualisation script
  myVisScript <- "#############################\n# start of Visualisation script\n#############################"

  if (nrOfVariables==2) {
    myVisScript <- append(myVisScript,
                          paste0("persp(",
                                 myVarNames[1],
                                 ",",
                                 myVarNames[2],
                                 ",mumax,col = 'green',xlab='",
                                 myVarNames[1],
                                 "',ylab='",
                                 myVarNames[2],
                                 "',zlab='mu_max',main='Response surface mu_max for\n",
                                 as.character(growthModels$Microorganism[run]),
                                 " in/on ",
                                 as.character(growthModels$Product[run]),
                                 "\n(gropin ID:",
                                 as.character(growthModels$ModelID[run]),
                                 ")",
                                 "',theta=305,phi=20,shade=0.25,ticktype = 'detailed')")
                          )
    }
  if (nrOfVariables==1){
    myVisScript <- append(myVisScript,
                   paste0("plot(",
                          myVarNames[1],
                          ",result,
                          xlab='",
                          myVarNames[1],
                          "',
                          ylab='mu_max')"))
  }
  myVisScript <- append(myVisScript,"#############################\n# End of Visualisation script\n#############################")

  
 
  
  
  ###################################################################
  # editing Annotation spreadsheet
  # but using 'as is' in order to avoid issues when loading into fskx
  ##################################################################
  MetaData <- read_excel("ModelAnnotationExceltemplateV1.04.xlsx", 
                         sheet = "Generic Metadata Schema")
  
  # mandatory fields
  #Name of the Model
  MetaData$Data[1] <-paste("Gropin growth model for",
                           as.character(growthModels$Microorganism[run]),
                           "in/on",
                           as.character(growthModels$Product[run]),
                           "(gropin ID:",
                           as.character(growthModels$ModelID[run]),
                           ")"
                           )
  # Identifier
  MetaData$Data[3] <-paste0("gropinID",
                           as.character(growthModels$ModelID[run]))
  # License
  MetaData$Data[8] <-"Academic Free License 3.0"
  
  # Language Written in
  MetaData$Data[26] <-"R 3"
  
  # parameters
  
  firstRow <- 132 # location in annotation sheet
  thisRow <- firstRow
  for(fskPar in 1:nrOfVariables){
    MetaData$...12[thisRow] <- myVarNames[fskPar]
    MetaData$...13[thisRow] <- "Input"
    MetaData$...14[thisRow] <- myVarNames[fskPar]
    MetaData$...15[thisRow] <- "my dummy description"
    MetaData$...16[thisRow] <- "no idea"
    MetaData$...17[thisRow] <- "dummy unit category"
    MetaData$...18[thisRow] <- "Vector[number]"
    MetaData$...19[thisRow] <- "my source"
    MetaData$...20[thisRow] <- "my subject"
    MetaData$...21[thisRow] <- "mydist"
    MetaData$...22[thisRow] <- valuesForParameters[fskPar]
    thisRow <- thisRow+1
  }
  if (namesOfCoeffsList[1]!='not used') {
    for(fskCoeff in 1:nrOfCoeffs){
      MetaData$...12[thisRow] <- namesOfCoeffs[fskCoeff]
      MetaData$...13[thisRow] <- "Constant"
      MetaData$...14[thisRow] <- namesOfCoeffs[fskCoeff]
      MetaData$...15[thisRow] <- "my dummy description"
      MetaData$...16[thisRow] <- "no idea"
      MetaData$...17[thisRow] <- "dummy unit category"
      MetaData$...18[thisRow] <- "Double"
      MetaData$...19[thisRow] <- "my source"
      MetaData$...20[thisRow] <- "my subject"
      MetaData$...21[thisRow] <- "mydist"
      MetaData$...22[thisRow] <- valuesOfCoeffs[fskCoeff]
      thisRow <- thisRow+1
    }
  }
  
  ##############################################################################
  # Creating files for fsk creator node in Knime
  ##############################################################################
  # store scripts to disk
  fileNamePar <- paste0("par/par",growthModels$ModelID[run],".r")
  fileConn<-file(fileNamePar)
  writeLines(myParScript, fileConn)
  close(fileConn)
  
  fileNameMod <- paste0("mod/mod",growthModels$ModelID[run],".r")
  fileConn<-file(fileNameMod)
  writeLines(myModelScript, fileConn)
  close(fileConn)
  
  fileNameVis <- paste0("vis/vis",growthModels$ModelID[run],".r")
  fileConn<-file(fileNameVis)
  writeLines(myVisScript, fileConn)
  close(fileConn)
  
  myFullScript <- myParScript
  myFullScript <- append(myFullScript,myModelScript)
  myFullScript <- append(myFullScript,myVisScript)
  
  fileNameFull <- paste0("fullScript",growthModels$ModelID[run],".r")
  fileConn<-file(fileNameFull)
  writeLines(myFullScript, fileConn)
  close(fileConn)
  
  fileNameSchema <- paste0("schema/metadataschema",growthModels$ModelID[run],".xlsx")
  write_xlsx(list("Generic Metadata Schema"=MetaData),path=fileNameSchema)
#}