#library(rjson)
library(zip)
library(stringr)

# for easy count ups
`%+=%` = function(e1,e2) eval.parent(substitute(e1 <- e1 + e2))

# project path / obsolete in source mode
thisProjectPath <- "C:/Users/Joker/Documents/Projects/ModelRepository/gropin/"
tempFolder <- "temp/"
sourceSubFolder <- "fskx/"
targetSubFolder <- "finishedFSKX/"
#targetSubFolder <- "testingstuff/"

#########################
# name place holder for new simulation settin names
newSimName <- "point_Value_For_"


myList <- list.files(file.path(thisProjectPath,sourceSubFolder),pattern="*.fskx")


# create target folder if not existing
if(!dir.exists(file.path(thisProjectPath,targetSubFolder))){
  dir.create(file.path(thisProjectPath,targetSubFolder))
}


# bring current date into format used by meta data schema
currentDate <- sub("0?(.+)-0?(.+)-0?(.+)", "\\1,\\2,\\3", Sys.Date())

# new metadata string
newCreationDate <- paste0(",\"creationDate\":[",
                          currentDate,
                          "],\"modificationDate\":[]")

#run <- 2
for(run in 1:length(myList)) {
  # check if folders exist
  if(!dir.exists(file.path(thisProjectPath,tempFolder))){
    dir.create(file.path(thisProjectPath,tempFolder))
  }
  
  zipSourceFile <- paste0(thisProjectPath,sourceSubFolder,myList[run])
  zipTargetFile <- paste0(thisProjectPath,targetSubFolder,myList[run])


  # put fskx file into temp folder
  unzip(zipSourceFile,exdir = file.path(thisProjectPath,tempFolder))

  ##############################################################
  # PUT YOUR CHANGES IN HERE
  ##############################################################
  ##### Creation Date ########
  # get metaData for creation Date
  oldData <- readLines(paste0(thisProjectPath,tempFolder,"metaData.json"))
  # replace no Creation date with current day
  # NOTE! works only if no creation date was entered!
  changedData <- gsub(pattern = ",\"rights\":\"", 
                  replace = paste0(newCreationDate,",\"rights\":\""), 
                  x = oldData)
  ##### Creation Date ########
  
  
  
  ##### Reference Edit ########
  # first find reference, stored in comments (design decision)
  capturedJournalInfo <- str_match(changedData, "comment\":\"\\s*(.*?)\\s*\"")
  journalVolumeIssue <- unlist(strsplit(capturedJournalInfo[2], ","))
  myString <- paste0("journal\":\"",
                     journalVolumeIssue[1],
                     "\",\"volume\":\"",
                     journalVolumeIssue[2],
                     "\",\"issue\":\"",
                     journalVolumeIssue[3],
                     "\",\"status\":\"Published\",",
                     "\"website\":\"https://www.aua.gr/psomas/gropin/\",",
                     "\"comment\":\"\",\"abstract\":\"\"")

  # replace placeholder comment (containing reference info)
  # with correct entries in forms, so that FSK-Lab may read this correctly
  # NOTE! works only, if gropin2R script stores info about 
  # journal,volume,issue in the comment section of this reference
  # TODO: extension step: creating the whole metadata.json file and all other 
  # necessary files for fskx in 1 R-script (takes more time to develop)
  newData <- gsub(pattern = str_match(changedData, "comment\":\"\\s*(.*?)\\s*\"")[1], 
                      replace = myString, 
                      x = changedData)
  ##### Reference Edit ########
 
  
  
  ##### New Simulation Settings ########
  #editing sim.sedml
  oldSim <- readLines(paste0(thisProjectPath,tempFolder,"sim.sedml"))
  
  
  # find the names and values of all variables
  originalValuesVariablesVec <- str_match(oldSim, "newValue=\"\\s*(.*?)\\s*\"")[,2]
  originalValuesVariablesVec <- originalValuesVariablesVec[!is.na(originalValuesVariablesVec)]
  originalNamesVariablesVec <- str_match(oldSim, "target=\"\\s*(.*?)\\s*\"")[,2]
  originalNamesVariablesVec <- originalNamesVariablesVec[!is.na(originalNamesVariablesVec)]
  
  # adding new simulation settings if nrOfVariables > 2 only: no reason for different setting
  if(length(originalNamesVariablesVec)>2) {
    
    # all permutation of pairs of parameters
    varPairs <- combn(originalNamesVariablesVec[!is.na(originalNamesVariablesVec)],2)
    nrOfPermutations <- dim(varPairs)[2] # should be choose(nrOfVariables,nrOfVariables-2)
    #par <- 1
  
    # marker for end of simulation settings
    #searchString1 <- "<listOfModels>\\s*(.*?)\\s*</listOfModels>"
    locationOfEdit <- which(gsub("[[:space:]]", "", oldSim) %in% "</listOfModels>")-1
    
   
    
    newSim <- oldSim
    
    #add new parameter settings
    for(sim in 1:nrOfPermutations) {
      # get names of variables that are point values for each simulation setting
      missingVar <- originalNamesVariablesVec[is.na(match(originalNamesVariablesVec[!is.na(originalNamesVariablesVec)],varPairs[,sim]))]
      missingVar <- missingVar[!is.na(missingVar)]
      
      nameOfSimulation <- paste0(newSimName,paste0(missingVar,collapse = '_'))
      newSim <- append(newSim,
                       paste0("<model id=\"",
                              nameOfSimulation,
                              "\" name=\"\" language=\"https://iana.org/assignments/mediatypes/text/x-r\" source=\"./model.r\">"),
                       after=locationOfEdit)
      locationOfEdit%+=%1
      newSim <- append(newSim,
                       "<listOfChanges>",
                       after=locationOfEdit)
    
      
      newValuesVariablesVec <- originalValuesVariablesVec
      idsOfPointValues <- which(originalNamesVariablesVec %in% missingVar)
      for(i in idsOfPointValues) newValuesVariablesVec[i] <- round(mean(eval(parse(text=originalValuesVariablesVec[i]))), digits=2)
      #newValuesVariablesVec[sim] <- round(mean(eval(parse(text=originalValuesVariablesVec[sim]))), digits=2)
      
      for(val in 1:length(originalNamesVariablesVec)) {
        locationOfEdit%+=%1
        newSim <- append(newSim,
                         paste0("<changeAttribute newValue=\"",
                                newValuesVariablesVec[val],
                                "\" target=\"",
                                originalNamesVariablesVec[val],
                                "\" />"),
                         after=locationOfEdit)
      }
      locationOfEdit%+=%1
      newSim <- append(newSim,
                     "</listOfChanges>",
                       after=locationOfEdit)
      locationOfEdit%+=%1
      newSim <- append(newSim,
                        "</model>",
                       after=locationOfEdit)
      locationOfEdit%+=%1
    }
    
    # edit tasks in correct line of file
    locationOfEdit <- which(gsub("[[:space:]]", "", newSim) %in% "</listOfTasks>")-1
    
    # add new tasks: number of new tasks is \binomial{nrOfVariables}over{nrOfVariables-2}
    for(sim in 1:length(originalNamesVariablesVec)) {
      nameOfSimulation <- paste0(newSimName,originalNamesVariablesVec[sim])
      newSim <- append(newSim,
                       paste0("<task id=\"task",
                              sim,
                              "\" name=\"\" modelReference=\"",
                              nameOfSimulation,
                              "\" simulationReference=\"steadyState\" />"),
                       after=locationOfEdit)
      locationOfEdit%+=%1
    }
    ##### New Simulation Settings ########
   
   
  
    ##### Edit Readme.txt ########
    oldReadMe <- readLines(paste0(thisProjectPath,tempFolder,"README.txt"))
    newReadMe <- oldReadMe
    newReadMe <- append(newReadMe,"## Response Surface Resolution")
    newReadMe <- append(newReadMe,"This auto-generated fskx file decreased the resolution of each variable vector depending on the number of variables.")
    newReadMe <- append(newReadMe,"Example: T <- seq(2.002,19.98001998002,length.out=10)")
    newReadMe <- append(newReadMe,"This script calculates the full data cube of all possible combinations of variables.")
    newReadMe <- append(newReadMe,"The computational effort increases therefore with number of variables, thus it was decided to decrease the length of each vector for models with number of variables >2.")
    newReadMe <- append(newReadMe,"The user may change it back to its original value in gropin:")
    newReadMe <- append(newReadMe,"Example: T <- seq(2.002,19.98001998002,length.out=21)")
    newReadMe <- append(newReadMe,"Please be mindful of the additional computational effort.")
    newReadMe <- append(newReadMe,"## Response Surface Resolution")
    
    ##### Edit Readme.txt ########
  }
  
  
  
  ##############################################################
  # ALL CHANGES HAVE TO BE DONE BY NOW
  ##############################################################
  
  ##### Storing changes into temp folder ########
  fileConn <- file(paste0(thisProjectPath,tempFolder,"sim.sedml"))
  writeLines(newSim,fileConn)
  close(fileConn)
  
  fileConn <- file(paste0(thisProjectPath,tempFolder,"metaData.json"))
  writeLines(newData,fileConn)
  close(fileConn)
  
  fileConn <- file(paste0(thisProjectPath,tempFolder,"README.txt"))
  writeLines(newReadMe,fileConn)
  close(fileConn)
  ##### Storing changes into temp folder ########

  
  zipr(zipTargetFile,
       file.path(thisProjectPath,
                 tempFolder,
                 list.files(file.path(thisProjectPath,tempFolder))),
      recurse=TRUE)
  
  # clean up to avoid data garbage
  unlink(file.path(thisProjectPath,tempFolder), recursive = TRUE)
  
  print(paste("done with file:",myList[run]))
}