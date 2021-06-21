################################################################################
# general workflow:
# phase 1: create scripts (par,model,visualisation,full)
#         & metadata schema as ingredients for FSK Creator Node
#     phase 1 is divided into 3 parts
#       1.  preprocessing
#       2.  creating scripts as series of strings (a.k.a. metacoding)
#       3.  Creating files for fsk creator
# phase 2: KNIME workflow for using FSK Creator node in a loop for all scripts
#         thus creating fskx files
# phase 3: fixing minor errors due to FSK creator issues 
#           e.g. 
#             creation date; 
#             journal, issue, volume in reference
#             additional simulations
#           
################################################################################

################################################################################
# setting a few parameters for potential later adaptations to gropin
################################################################################

# length of variable ranges of different models depending on the nr of variables
# to save computational effort. position in this vector relates to nrOfVariables of model
lenOfVarVec <- c(21,21,10,7,5,4,3,3,2,2)
#lenOfVarVec <- rep(21,10)  # legacy length of sequence vector of variables (equal to gropin)

# names of subfolders for different scripts and files
subfolderParScript <- "par"
subfolderModelScript <- "mod"
subfolderVisScript <- "vis"
subfolderSchemaScript <- "schema"
subfolderFullscript <- "fullscript"

# project path / obsolete in source mode
thisProjectPath <- "~/Projects/ModelRepository/gropin"

# gropin DB source
gropinDBfilename <- "GroPIN201031.xlsm"

# metadataschema parameter to fill in
metaDataSchemaFilename <- "ModelAnnotationExceltemplateV1.04.xlsx"
familyNameOfCreator <- "Fuhrmann"
firstNameOfCreator <- "Marcel"
emailOfCreator <- "marcel.fuhrmann@bfr.bund.de"
organizationOfCreator <- "BfR"

# adding new output parameters (just add another component to each vector of strings)
idsOfOutputPar <- c("responseSurface")
namesOfOutputPar <- c("data frame with variables and corresponding mumax")
descriptionOfOutputPar <- c("This dataframe consists of a number of columns 
                            in relation to the number of variables of this 
                            model. One additional column contains the response 
                            surface mu_max result based on this secondary model.")
unitsOfOutputPar <- c("[]")
datatypeOfOutputPar <- c("Matrix[number,number]")

###################################################
# bugfixing parameters, non functional for transfer
###################################################
# finding weird models with lots of exceptions
# than one wants to exclude until further notice
listOfNonfunctioningModels <- c(263,264,28,128,331,332)
################################################################################

###################################################
# extension phase 1: meta data info of 2var models
###################################################
idsOfVars <- c("aw",
               "bw",
               "CLO",
               "CO2",
               "days",
               "Irradiationdose",
               "Limonin",
               "NaCl",
               "NO2",
               "O2",
               "pH",
               "PL_SDAmix",
               "S",
               "Sugar",
               "T",
               "Ac",
               "AscorbA",
               "Citra",
               "CO2_dissolved_",
               "Ethanol",
               "Fructose",
               "Gelatin",
               "Hours",
               "NaNO2",
               "Oleo",
               "Phe",
               "S_S")
namesOfVars <- c("water activity",
                 "name bw",
                 "name CLO",
                 "Carbon dioxide",
                 "storage days",
                 "Irradiation dose",
                 "name Limonin",
                 "name Salt",
                 "name NO2",
                 "Oxygen",
                 "name pH",
                 "name PL_SDAmix",
                 "name S",
                 "name Sugar",
                 "Temperature",
                 "name Ac",
                 "name AscorbA",
                 "name Citra",
                 "name CO2_dissolved_",
                 "name Ethanol",
                 "name Fructose",
                 "name Gelatin",
                 "name Hours",
                 "name NaNO2",
                 "name Oleo",
                 "name Phe",
                 "name S_S")
descriptionsOfVars <- c("descr water activity",
                       "descr bw",
                       "descr CLO",
                       "descr CO2",
                       "descr days",
                       "descr Irradiationdose",
                       "descr Limonin",
                       "descr NaCl",
                       "descr NO2",
                       "descr O2",
                       "descr pH",
                       "descr PL_SDAmix",
                       "descr S",
                       "descr Sugar",
                       "descr Temperature",
                       "descr Ac",
                       "descr AscorbA",
                       "descr Citra",
                       "descr CO2_dissolved_",
                       "descr Ethanol",
                       "descr Fructose",
                       "descr Gelatin",
                       "descr Hours",
                       "descr NaNO2",
                       "descr Oleo",
                       "descr Phe",
                       "descr S_S")
unitsOfVars <- c("[%]",
                 "unit bw",
                 "unit CLO",
                 "kg",
                 "unit days",
                 "unit Irradiationdose",
                 "unit Limonin",
                 "unit NaCl",
                 "unit NO2",
                 "unit O2",
                 "unit pH",
                 "unit PL_SDAmix",
                 "unit S",
                 "unit Sugar",
                 "C",
                 "unit Ac",
                 "unit AscorbA",
                 "unit Citra",
                 "unit CO2_dissolved_",
                 "unit Ethanol",
                 "unit Fructose",
                 "unit Gelatin",
                 "unit Hours",
                 "unit NaNO2",
                 "unit Oleo",
                 "unit Phe",
                 "unit S_S")
unitcategorysOfVars <- c("Dimensionless Parameter",
                         "unit category bw",
                         "unit category CLO",
                         "unit category CO2",
                         "time",
                         "unit category Irradiationdose",
                         "unit category Limonin",
                         "concentration",
                         "unit category NO2",
                         "unit category O2",
                         "Dimensionless Parameter",
                         "unit category PL_SDAmix",
                         "unit category S",
                         "unit category Sugar",
                         "Temperature",
                         "unit category Ac",
                         "unit category AscorbA",
                         "unit category Citra",
                         "unit category CO2_dissolved_",
                         "unit category Ethanol",
                         "unit category Fructose",
                         "unit category Gelatin",
                         "unit category Hours",
                         "unit category NaNO2",
                         "unit category Oleo",
                         "unit category Phe",
                         "unit category S_S")
###################################################

# set your own path
setwd(thisProjectPath)
#setwd(dirname(sys.frame(1)$ofile)) # current working directory(only works in source mode)



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
gropinDB <- read_excel(gropinDBfilename, sheet = "Nonlinear") # current version

# replace some special characters
names(gropinDB)<-gsub("/","_",names(gropinDB))
# replace "(...string....)" but not in the equation column (column 95 aka last)
gropinDB[,1:94] <- data.frame(lapply(gropinDB[,1:94], 
                                     function(x) {gsub('[()]', '_', x)}))
gropinDB[,1:94] <- data.frame(lapply(gropinDB[,1:94], 
                                     function(x) {gsub('[/]', '_', x)}))
gropinDB[,1:94] <- data.frame(lapply(gropinDB[,1:94], 
                                     function(x) {gsub('[+]', '_', x)}))
gropinDB[,1:94] <- data.frame(lapply(gropinDB[,1:94], 
                                     function(x) {gsub('[+]', '_', x)}))



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


#create list of all possible variables
existingVariables <- table(cbind(as.character(growthModels$Var1),
                                 as.character(growthModels$Var2),
                                 as.character(growthModels$Var3),
                                 as.character(growthModels$Var4),
                                 as.character(growthModels$Var5),
                                 as.character(growthModels$Var6),
                                 as.character(growthModels$Var7),
                                 as.character(growthModels$Var8),
                                 as.character(growthModels$Var9),
                                 as.character(growthModels$Var10)))
allVariables <- as.character(data.frame(existingVariables)$Var1)

################################################################################
# END PART ONE: Preprocessing 
################################################################################
#run <- 61
#run <- 254
#run <- 482
#run <- 22
#run <- 28
#run <- 1

################################################################################
# START PART TWO: creating scripts
################################################################################
nrModels <- dim(growthModels)[1]

#initialising
model2Vars <- NA

for(run in 1:nrModels){
  
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
  myVarNames <- gsub("[+/-]","",myVarNames)
  myVarNames <- gsub(" ","",myVarNames)
  
  # phrase of gropin, marks end of list of variables
  if("notused" %in% myVarNames) {
    myVarNames[which(myVarNames %in% "notused")] <- NA
  }
  
  
  # different models have different number of variables (rest is NA)
  nrOfVariables <- length(table(myVarNames))
 
  #collecting unique variables from models with less than 3 variables
  # for 1st extension step, adding metadata info to those 2-variable models
  # if(nrOfVariables<4){
  #   print("already done")
  #   next
  # }
  if(nrOfVariables>=3){
    model2Vars <- append(model2Vars,myVarNames)
  }
  
  
  # placeholder for different stages
#  if(nrOfVariables>6){
 #   print("wait for extension step 3")
  #  next
  #}
  
  # note that few models have zeros in rare cases 
  # in the denominator of a fraction term
  # solution: set boundaries of sequence to inner boundaries to catch cases
  # where subtraction of variables and coefficients in denominator happen
  # TODO check if factorisation with 1.01*myVarMin and 0.99*myVarMax obsolete 
  # in future versions of gropin
  shiftingRangeOfVariablesSlightly <- 1.001
  # store range info of variables
  myVarMin <- c(as.double(as.character(growthModels$Var1Min[run])),
                as.double(as.character(growthModels$Var2Min[run])),
                as.double(as.character(growthModels$Var3Min[run])),
                as.double(as.character(growthModels$Var4Min[run])),
                as.double(as.character(growthModels$Var5Min[run])),
                as.double(as.character(growthModels$Var6Min[run])),
                as.double(as.character(growthModels$Var7Min[run])),
                as.double(as.character(growthModels$Var8Min[run])),
                as.double(as.character(growthModels$Var9Min[run])),
                as.double(as.character(growthModels$Var10Min[run])))*shiftingRangeOfVariablesSlightly
  myVarMax <- c(as.double(as.character(growthModels$Var1Max[run])),
                as.double(as.character(growthModels$Var2Max[run])),
                as.double(as.character(growthModels$Var3Max[run])),
                as.double(as.character(growthModels$Var4Max[run])),
                as.double(as.character(growthModels$Var5Max[run])),
                as.double(as.character(growthModels$Var6Max[run])),
                as.double(as.character(growthModels$Var7Max[run])),
                as.double(as.character(growthModels$Var8Max[run])),
                as.double(as.character(growthModels$Var9Max[run])),
                as.double(as.character(growthModels$Var10Max[run])))/shiftingRangeOfVariablesSlightly
  
  # preprocess nonsensical entries of minimum and maximum of variable range
  # all entries after nrOfVariables is reached are considered nonsensical
  myVarMin[nrOfVariables+1:length(myVarMin)] <- NA
  myVarMin[nrOfVariables+1:length(myVarMin)] <- NA
  
  # for cases where min & max was put in wrong order in gropin
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
                      lenOfVarVec[nrOfVariables],
                      ")")
    )
    
    # create vector of input values for annotation schema later
    valuesForParameters[j] <- paste0("seq(" , 
                                  myVarMin[j], 
                                  ",",
                                  myVarMax[j],
                                  ",length.out=",
                                  lenOfVarVec[nrOfVariables],
                                  ")")
    
   }
  
  
  
 
  
 
  myParScript <- append(myParScript,"#############################\n# end of Parameter script\n#############################")
  
  ##############################################################################
  # end of setting parameters
  ##############################################################################
  
  
  ##############################################################################
  # Start model script
  ##############################################################################
  
  # comments to identify for model script
  myModelScript <- paste("#############################",
                         "\n# start of Model script Gropin ID",
                         growthModels$ModelID[run],
                         "\n#############################")

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
  # function "toupper" ensures that all gropin variables are upper case
  # important for replacement to variable
  myEq <- gsubfn("\\w+",as.list(setNames(myVarNames[1:nrOfVariables],
                                         gropinVarNames[1:nrOfVariables])),
                 toupper(growthModels$equation[run]))

  # gropin uses excel functions to make certain calculations
  # in order to convert each line to R, these function names
  # need to be corrected
  # SOLUTION: collect the names of excel functions and
  # the names of R functions in a list
  gropinFunctionNames <- c("SQRT","EXP","LN","ln")
  FunctionNames <- c("sqrt","exp","log","log")
  
  # replacing gropin function names with R function names
  myEq <- gsubfn("\\w+",as.list(setNames(FunctionNames,gropinFunctionNames)),myEq)
  
  
  myModelScript <- append(myModelScript," ")
  myModelScript <- append(myModelScript,"# constant coefficients for this model")
  
  # Entering coefficients hardcoded into model script
  # different models have different coefficients 
  namesOfCoeffsList<-growthModels[run,names(growthModels)[53:92][c(TRUE,FALSE)]]
  valuesOfCoeffsList<-growthModels[run,names(growthModels)[53:92][c(FALSE,TRUE)]]
  
  # extracting actual names and values only and remove potential special characters
  if (namesOfCoeffsList[1]!='not used') {
    namesOfCoeffs <- as.character(unlist(namesOfCoeffsList))[!is.na(as.character(unlist(namesOfCoeffsList)))]
    namesOfCoeffs <- gsub("[+]","",namesOfCoeffs)
    namesOfCoeffs <- gsub("[[:punct:]]","",namesOfCoeffs)
    namesOfCoeffs <- gsub(" ","",namesOfCoeffs)
    valuesOfCoeffs <- as.character(unlist(valuesOfCoeffsList))[!is.na(as.character(valuesOfCoeffsList))]
    valuesOfCoeffs <- gsub("_","",valuesOfCoeffs)
    nrOfCoeffs <- length(namesOfCoeffs)
    
    for (c in 1:nrOfCoeffs) {
      myModelScript <- append(myModelScript,paste0(namesOfCoeffs[c]," <- ",valuesOfCoeffs[c]))
    }
    
  }
  
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
                          "argumentsPar <- unique.data.frame(expand.grid(variables))"
  )
  
  
  # here is the equation to calculate mumax added to the script
  myModelScript <- append(myModelScript," ")
  myModelScript <- append(myModelScript,"# heart of the model")
  myModelScript <- append(myModelScript,
                          paste0("response_surface <- function(",
                                 paste(myVarNames[1:nrOfVariables],collapse = ','),
                             ") {\n   mumax <-",
                             myEq))
  myModelScript <- append(myModelScript,"\n\treturn(mumax=mumax)\n} ")
  myModelScript <- append(myModelScript,"\n# output parameters")
  myModelScript <- append(myModelScript, 
                          paste0("responseSurface <- cbind(argumentsPar,response_surface(",
                                 paste0("argumentsPar['",myVarNames[1:nrOfVariables],"']",collapse = ','),
                          "))",
                          "\ncolnames(responseSurface) <- c(colnames(argumentsPar),'",
                          gsub("[[:punct:]]","",as.character(growthModels$mumax[run])),
                          "')")
                          )
  myModelScript <- append(myModelScript,"#############################\n# End of Model script\n#############################")
  
  ##############################################################################
  # End of Model Scripting
  ##############################################################################
  
  ##############################################################################
  # Start of Visualisation Scripting
  ##############################################################################
  
  # comments to identify for Visualisation script
  myVisScript <- paste("#############################",
                       "\n# start of Visualisation script Gropin ID",
                       growthModels$ModelID[run],
                       "\n#############################")

  myVisScript <- append(myVisScript,paste0("titleText <-",
                                           "'Response surface ",
                                           gsub("[[:punct:]]","_",as.character(growthModels$mumax[run])),
                                           " for\n",
                                           as.character(growthModels$Microorganism[run]),
                                           " in/on ",
                                           as.character(growthModels$Product[run]),
                                           "\n(gropin ID:",
                                           as.character(growthModels$ModelID[run]),
                                           ")'"))
  
  if (nrOfVariables>=3) {
    
    # for models with 3 or more variables, a special visualisation was devised:
    # 3 plots of the first 3 variables are combined into 1 plot
    varPairs <- combn(myVarNames[!is.na(myVarNames)],2)
    
    for(par in 1:3) {
      
      myVisScript <- append(myVisScript,paste0("argPar",
                                               par,
                                               " <- unique.data.frame(expand.grid(",
                                               varPairs[1,par],
                                               ",",
                                               varPairs[2,par],
                                               "))"))
    }
    
    for(par in 1:3) {

      # get axis of point values
      missingVar <- myVarNames[is.na(match(myVarNames[!is.na(myVarNames)],varPairs[,par]))]
      missingVar <- missingVar[!is.na(missingVar)]
      
      myVisScript <- append(myVisScript,paste0("z",
                                               par,
                                               " <- matrix(unlist(response_surface(",
                                               varPairs[1,par],
                                               " = argPar",
                                               par,
                                               "[1],",
                                               varPairs[2,par],
                                               " = argPar",
                                               par,
                                               "[2],",
                                               paste0(missingVar," = ",missingVar,"[",1,"]",collapse=','),
                                               ")),nrow=",
                                               lenOfVarVec[nrOfVariables],
                                               ")"))
    }

  #  if(nrOfVariables==3){
      # myVisScript <- append(myVisScript,paste0("z2 <- matrix(unlist(response_surface(argPar2[1],",
      #                                          myVarNames[2],
      #                                          "[",
      #                                          1,
      #                                          "],argPar2[2])),nrow=",
      #                                          lenOfVarVec[nrOfVariables],
      #                                          ")"))
      # myVisScript <- append(myVisScript,paste0("z3 <- matrix(unlist(response_surface(",
      #                                          myVarNames[1],
      #                                          "[",
      #                                          1,
      #                                          "],argPar3[1],argPar3[2])),nrow=",
      #                                          lenOfVarVec[nrOfVariables],
      #                                          ")"))
   # }
    # if(nrOfVariables>3) {
    #   myVisScript <- append(myVisScript,paste0("z2 <- matrix(unlist(response_surface(argPar2[1],",
    #                                            myVarNames[2],
    #                                            "[",
    #                                            1,
    #                                            "],argPar2[2],",
    #                                            paste0(myVarNames[4:nrOfVariables],"[",1,"]",collapse=','),
    #                                            ")),nrow=",
    #                                            lenOfVarVec[nrOfVariables],
    #                                            ")"))
    #   myVisScript <- append(myVisScript,paste0("z3 <- matrix(unlist(response_surface(",
    #                                            myVarNames[1],
    #                                            "[",
    #                                            1,
    #                                            "],argPar3[1],argPar3[2],",
    #                                            paste0(myVarNames[4:nrOfVariables],"[",1,"]",collapse=','),
    #                                            ")),nrow=",
    #                                            lenOfVarVec[nrOfVariables],
    #                                            ")"))
    # }
    
    myVisScript <- append(myVisScript,"# adding precaution if response surface is zero")
    for(par in 1:3) {
      myVisScript <- append(myVisScript,paste0("myZLim",
                                               par,
                                               " <- range(z",
                                               par,
                                               ")"))
      
      myVisScript <- append(myVisScript,paste0("if(myZLim",
                                               par,
                                               "[1] == myZLim",
                                               par,
                                               "[2]) myZLim",
                                               par,
                                               "[2] <- myZLim",
                                               par,
                                               "[2]+1"))
    }
    
    myVisScript <- append(myVisScript,paste0("if(length(",
                                             paste0(myVarNames[1:nrOfVariables],collapse = ")>1 & length("),
                                             ")>1) {"))
    myVisScript <- append(myVisScript,"\tpar(mfrow = c(1,3))")
    
    for(par in 1:3) {
      myVisScript <- append(myVisScript,paste0("\tpersp(",
                                               varPairs[1,par],
                                               ",",
                                               varPairs[2,par],
                                               ",z",
                                               par,
                                               ",col = 'green',xlab='",
                                               varPairs[1,par],
                                               "',ylab='",
                                               varPairs[2,par],
                                               "',zlab='",
                                               gsub("[[:punct:]]","_",as.character(growthModels$mumax[run])),
                                               "',zlim=myZLim",
                                               par,
                                               ",theta=305,phi=20,shade=0.25,ticktype = 'detailed')"))
    }
    myVisScript <- append(myVisScript,paste0("\tmtext(titleText,outer=T,  cex=1.2, line=-8.5, side=3)"))
    myVisScript <- append(myVisScript,"} else {")

    myVisScript <- append(myVisScript,paste0("\t\tmyPars <- unique.data.frame(expand.grid(",
                                             paste0(myVarNames[!is.na(myVarNames)], collapse = ','),
                                             "))"))
    myVisScript <- append(myVisScript,paste0("\t\tmyZ <-",
                                             "matrix(unlist(response_surface(",
                                             paste0("myPars[,",1:nrOfVariables,"]",collapse = ','),
                                             ")),nrow=",
                                             lenOfVarVec[nrOfVariables],
                                             ")"))
    myVisScript <- append(myVisScript,"\t\tmyZLim <- range(myZ)")
    
    myVisScript <- append(myVisScript,"if(myZLim[1] == myZLim[2]) myZLim[2] <- myZLim[2]+1")
    
        
    nrOfPermutations <- dim(varPairs)[2]
    for(par in 1:nrOfPermutations) {
      # getting the variable names which are not a vector
      missingVar <- myVarNames[is.na(match(myVarNames[!is.na(myVarNames)],varPairs[,par]))]
      missingVar <- missingVar[!is.na(missingVar)]
      
      myVisScript <- append(myVisScript,paste0("\tif(length(",
                                               paste0(missingVar,collapse = ')==1 & length('),
                                               ")==1) {"))
      myVisScript <- append(myVisScript,paste0("\t\tpersp(",
                                               varPairs[1,par],
                                               ",",
                                               varPairs[2,par],
                                               ",myZ",
                                               ",col = 'green',xlab='",
                                               varPairs[1,par],
                                               "',ylab='",
                                               varPairs[2,par],
                                               "',zlab='",
                                               gsub("[[:punct:]]","_",as.character(growthModels$mumax[run])),
                                               "',main=titleText,sub=",
                                               paste0("paste('other variable: ",
                                                      paste0(missingVar," =',round(",missingVar,",digits = 2)",collapse = ", '"),
                                                      ")"),
                                               ",zlim=myZLim",
                                               ",theta=305,phi=20,shade=0.25,ticktype = 'detailed')"))
      myVisScript <- append(myVisScript,"\t}")
    }
    myVisScript <- append(myVisScript,"}")
  }
  
  if (nrOfVariables==2) {
    myVisScript <- append(myVisScript,
                          paste0("persp(",
                                 myVarNames[1],
                                 ",",
                                 myVarNames[2],
                                 ",matrix(unlist(responseSurface$'",
                                 gsub("[[:punct:]]","",as.character(growthModels$mumax[run])),
                                 "'),nrow=",
                                 lenOfVarVec[nrOfVariables],
                                 "),col = 'green',xlab='",
                                 myVarNames[1],
                                 "',ylab='",
                                 myVarNames[2],
                                 "',zlab='",
                                 gsub("[[:punct:]]","",as.character(growthModels$mumax[run])),
                                 "',main=titleText,theta=305,phi=20,shade=0.25,ticktype = 'detailed')")
                          )
    }
  if (nrOfVariables==1){
    myVisScript <- append(myVisScript,
                   paste0("plot(",
                          myVarNames[1],
                          ",responseSurface$'",
                          gsub("[[:punct:]]","",as.character(growthModels$mumax[run])),
                          "',xlab='",
                          myVarNames[1],
                          "',
                          ylab='",
                          gsub("[[:punct:]]","",as.character(growthModels$mumax[run])),
                          "',main=titleText)"))
  }
  myVisScript <- append(myVisScript,"#############################\n# End of Visualisation script\n#############################")

  
 
  
  
  ###################################################################
  # editing Annotation spreadsheet
  # but using 'as is' in order to avoid issues when loading into fskx
  ##################################################################
  MetaData <- read_excel(metaDataSchemaFilename, 
                         sheet = "Generic Metadata Schema")
  
  # mandatory fields
  #Name of the Model
  MetaData$Data[1] <-paste("Gropin secondary growth model for",
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
  
  # Language 
  MetaData$Data[25] <-"R"
  
  # Language Written in
  MetaData$Data[26] <-"R 3"
  
  # Model Category
  MetaData$Data[27] <-"Predictive"
  
  # Objective
  DependentVariable <- "mu_max"
  MetaData$Data[33] <-paste0("This model predicts and visualize the ",
                             DependentVariable, 
                            " of ", 
                            as.character(growthModels$Microorganism[run]),
                            " in ", 
                            as.character(growthModels$Product[run]),
                            " with the independent variable(s) ",
                            paste(myVarNames[!is.na(myVarNames)],collapse=", "),
                            " according to the publication from ",
                            as.character(growthModels$Authors[run]),
                            " on ",
                            as.character(growthModels$Paper[run]),
                            ". ")
  
  # Description
  MetaData$Data[34] <- paste("This model and all metadata included have been",
                             "automatically generated from the GroPIN microbial",
                             "modelling DataBase (https://www.aua.gr/psomas/gropin/,",
                             "version 2020). The model code has been converted",
                             "from Excel to R and the model itself is provided",
                             "as an FSKX file. This FSKX model contains also an",
                             "R script to visualize model-based prediction",
                             "results similar to those visualizations",
                             "provided by the GroPIN software. A user of the",
                             "FSKX model can provide user-defined values for",
                             "all model input parameters, some of them",
                             "specifically introduced to customize the generated visualization.")

  # list of Authors
  # transform string into list of family names and firstname abbreviations
  # remove year at the end of each author list
  listOfAuthors <- head(unlist(strsplit(as.character(growthModels$Authors[run]), ",")),-1)
  listOfAuthorsFamilyName <- gsub(" ","",listOfAuthors[c(TRUE,FALSE)], fixed = TRUE)
  listOfAuthorsGivenName <- gsub(" ","",listOfAuthors[c(FALSE,TRUE)],fixed = TRUE)
  
  for(nrAuthor in 1:length(listOfAuthorsFamilyName)) {
    rowID <- 2+nrAuthor
    MetaData$...31[rowID] <- listOfAuthorsFamilyName[nrAuthor]
    MetaData$...29[rowID] <- listOfAuthorsGivenName[nrAuthor]
    MetaData$...34[rowID] <- "none given"
  }
  
  # list of creators
  # TODO for future: multiple creators:
  # entries only in first row of creators, since as of now this is a one-person-team
  MetaData$...15[3] <- familyNameOfCreator
  MetaData$...13[3] <- firstNameOfCreator
  MetaData$...18[3] <- emailOfCreator
  MetaData$...16[3] <- organizationOfCreator
  
  #given references, only 1 reference per model given
  # is reference?
  MetaData$Creator[14] <- "Yes"
  #TYPE	
  #DATE	
  #MetaData$...13[14] <- "05.05.2021"
  #PUBMED 
  #DOI 
  MetaData$...15[14] <- "none given"
  #AUTHOR 
  MetaData$...16[14] <- paste(listOfAuthors, collapse = ", ")
  #LIST	
  #TITLE	
  MetaData$...17[14] <- as.character(growthModels$Paper[run])
  #ABSTRACT	
  #JOURNAL  #VOLUME	 #ISSUE	
  MetaData$...19[14] <- paste(as.character(growthModels$Journal[run]),
                              ", ",
                              as.character(growthModels$Issue[run]))
  #STATUS	
  #WEBSITE	
  #COMMENT	
  MetaData$...22[14] <- paste(as.character(growthModels$Journal[run]),
                              ", ",
                              as.character(growthModels$Issue[run]))
  
  
  # list of products currently 1 product per model
  # Product name
  MetaData$Creator[38] <- as.character(growthModels$Product[run])
  # product unit
  MetaData$...13[38] <- "none given"
  
  # Hazard name
  MetaData$...23[38] <- as.character(growthModels$Microorganism[run])
  # Hazard unit
  MetaData$...25[38] <- "log10(CFU)"
  
  
  
  # list of parameters
  firstRow <- 132 # location in annotation sheet
  thisRow <- firstRow
  for(fskPar in 1:nrOfVariables){
    #at the beginning of this script a number of variables
    #and the corresponding meta data information is stored
    # first step, find out which parameter are we talking about
    mdID <- which(idsOfVars %in% myVarNames[fskPar])
    
    #ID
    MetaData$...12[thisRow] <- myVarNames[fskPar]
    #classification
    MetaData$...13[thisRow] <- "Input"
    #name
    if (!is.element(myVarNames[fskPar],idsOfVars)) { 
      mdID <- 1
    }
    
    MetaData$...14[thisRow] <- namesOfVars[mdID]
    #description
    MetaData$...15[thisRow] <- descriptionsOfVars[mdID]
    #unit
    MetaData$...16[thisRow] <- unitsOfVars[mdID]
    #unit category
    MetaData$...17[thisRow] <- unitcategorysOfVars[mdID]
    #data type
    MetaData$...18[thisRow] <- "Vector[number]"
    # source
    MetaData$...19[thisRow] <- ""
    # subject
    MetaData$...20[thisRow] <- ""
    # distribution
    MetaData$...21[thisRow] <- ""
    # default value
    MetaData$...22[thisRow] <- valuesForParameters[fskPar]
    thisRow <- thisRow+1
  }
 
  nrOfOutputPars <- length(idsOfOutputPar)
  for(outPar in 1:nrOfOutputPars){
    MetaData$...12[thisRow] <- idsOfOutputPar[outPar]
    MetaData$...13[thisRow] <- "Output"
    MetaData$...14[thisRow] <- namesOfOutputPar[outPar]
    MetaData$...15[thisRow] <- descriptionOfOutputPar[outPar]
    MetaData$...18[thisRow] <- datatypeOfOutputPar[outPar]
    MetaData$...16[thisRow] <- unitsOfOutputPar[outPar]
  }
   
  
  
  
  ##############################################################################
  # Part 3: Creating files for fsk creator node in Knime
  ##############################################################################
  # store scripts to disk
  fileNamePar <- paste0(subfolderParScript,"/par",growthModels$ModelID[run],".r")
  fileConn<-file(fileNamePar)
  writeLines(myParScript, fileConn)
  close(fileConn)
  
  fileNameMod <- paste0(subfolderModelScript,"/mod",growthModels$ModelID[run],".r")
  fileConn<-file(fileNameMod)
  writeLines(myModelScript, fileConn)
  close(fileConn)
  
  fileNameVis <- paste0(subfolderVisScript,"/vis",growthModels$ModelID[run],".r")
  fileConn<-file(fileNameVis)
  writeLines(myVisScript, fileConn)
  close(fileConn)
  
  # create full script for bugfixing purposes
  myFullScript <- myParScript
  myFullScript <- append(myFullScript,myModelScript)
  myFullScript <- append(myFullScript,myVisScript)
  
  fileNameFull <- paste0(subfolderFullscript,"/fullScript",growthModels$ModelID[run],".r")
  fileConn<-file(fileNameFull)
  writeLines(myFullScript, fileConn)
  close(fileConn)
  
  fileNameSchema <- paste0(subfolderSchemaScript,"/metadataschema",growthModels$ModelID[run],".xlsx")
  write_xlsx(list("Generic Metadata Schema"=MetaData),path=fileNameSchema)
  print(paste("done with Model Nr.",growthModels$ModelID[run]))
}

all2VarModels <- as.character(data.frame(table(model2Vars))$model2Vars)