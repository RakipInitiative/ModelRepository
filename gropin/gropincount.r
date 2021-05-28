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
#run <-32


################################################################################
# START PART TWO: creating scripts
################################################################################
nrModels <- dim(growthModels)[1]
nrOfVariables <- rep(NA,nrModels)

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
  myVarNames <- gsub("[+]","",myVarNames)
  myVarNames <- gsub(" ","",myVarNames)

  # phrase of gropin, marks end of list of variables
  if("notused" %in% myVarNames) {
    myVarNames[which(myVarNames %in% "notused")] <- NA
  }


  # different models have different number of variables (rest is NA)
  nrOfVariables[run] <- length(table(myVarNames))
}
  
  