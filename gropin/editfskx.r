#library(rjson)
library(zip)
library(stringr)

# project path / obsolete in source mode
thisProjectPath <- "C:/Users/Joker/Documents/Projects/ModelRepository/gropin/"
tempFolder <- "temp/"
sourceSubFolder <- "fskx/"
#targetSubFolder <- "finishedFSKX/"
targetSubFolder <- "testingstuff/"

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

run <- 1
#for(run in 1:length(myList)) {
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
  # get metaData for creation Date
  oldData <- readLines(paste0(thisProjectPath,tempFolder,"metaData.json"))
  # replace no Creation date with current day
  # NOTE! works only if no creation date was entered!
  changedData <- gsub(pattern = ",\"rights\":\"", 
                  replace = paste0(newCreationDate,",\"rights\":\""), 
                  x = oldData)

  # editing reference
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
  
  
  #editing sim.sedml
  # TODO extract values qq<-str_match(oldSim, "newValue=\"\\s*(.*?)\\s*\"")[,2]
  #> qq[!is.na(qq)]
  # mean eval(parse(text=qq1))
  # insert new values
  # edit task id and so on
  testthisa <- "testme"
  oldSim <- readLines(paste0(thisProjectPath,tempFolder,"sim.sedml"))
  
  # find the names values of all variables
  originalValuesVariablesVec <- str_match(oldSim, "newValue=\"\\s*(.*?)\\s*\"")[,2]
  originalValuesVariablesVec <- originalValuesVariablesVec[!is.na(originalValuesVariablesVec)]
  originalNamesVariablesVec <- str_match(oldSim, "target=\"\\s*(.*?)\\s*\"")[,2]
  originalNamesVariablesVec <- originalNamesVariablesVec[!is.na(originalNamesVariablesVec)]
  
  
  # marker for end of simulation settings
  searchString1 <- "<listOfModels>\\s*(.*?)\\s*</listOfModels>"
  listOfSimulations <- str_match(paste0(oldSim,collapse = ''), searchString1)[2]
  listOfSimulations <- append(listOfSimulations,"<listOfModels>",after = 0)
  for(sim in 1:length(originalNamesVariablesVec)) {
    nameOfSimulation <- paste("point value for",originalNamesVariablesVec[sim])
    newValuesVariables <- originalValuesVariablesVec
    #just change 1 variable for each simulation setting
    newValuesVariables[sim] <- mean(eval(parse(text=originalValuesVariablesVec[sim])))
    
    newSim <- paste0("<model id=\"",
                     nameOfSimulation,
                     "\" name=\"\" language=\"https://iana.org/assignments/mediatypes/text/x-r\" source=\"./model.r\">\n\t",
                     "<listOfChanges>\n\t\t",
                     "<changeAttribute newValue=\"",
                     newValuesVariables[1],
                     "\" target=\"",
                     originalNamesVariablesVec[1],
                     "\" />\n\t\t",
                     "<changeAttribute newValue=\"",
                     newValuesVariables[2],
                     "\" target=\"",
                     originalNamesVariablesVec[2],
                     "\" />\n\t\t",
                     "<changeAttribute newValue=\"",
                     newValuesVariables[3],
                     "\" target=\"",
                     originalNamesVariablesVec[3],
                     "\" />\n\t",
                     "</listOfChanges>\n</model>\n")
    listOfSimulations <- append(listOfSimulations,newSim)
  }
  listOfSimulations <- append(listOfSimulations,"</listOfModels>")
  newSim1 <- gsub(pattern = searchString1, 
                 replace = paste(listOfSimulations,collapse=''), 
                 x = paste(oldSim,collapse=''))

  
  searchString2 <- "<listOfTasks>\\s*(.*?)\\s*</listOfTasks>"
  listOfTasks <- str_match(paste0(newSim1,collapse = ''), searchString2)[2]
  listOfTasks <- append(listOfTasks,"<listOfTasks>",after = 0)
  
  for(sim in 1:length(originalNamesVariablesVec)) {
    nameOfSimulation <- paste("point value for",originalNamesVariablesVec[sim])
    newTaskIDs <- paste0("task",sim)
    
    newSim2 <- paste0("\t<task id=\"",
                      newTaskIDs,
                     "\" name=\"\" modelReference=\"",
                     nameOfSimulation,
                     "\" simulationReference=\"steadyState\" />")
    listOfTasks <- append(listOfTasks,newSim2)
  }
  listOfTasks <- append(listOfTasks,"</listOfTasks>")
  newSim2 <- gsub(pattern = searchString2, 
                 replace = paste(listOfTasks,collapse=''), 
                 x = paste(newSim1,collapse=''))
  
  

  ##############################################################
  # ALL CHANGES HAVE TO BE DONE BY NOW
  ##############################################################
  
  
  fileConn <- file(paste0(thisProjectPath,tempFolder,"sim.sedml"))
  writeLines(newSim2,fileConn)
  close(fileConn)
  
  fileConn <- file(paste0(thisProjectPath,tempFolder,"metaData.json"))
  writeLines(newData,fileConn)
  close(fileConn)

  zipr(zipTargetFile,
       file.path(thisProjectPath,
                 tempFolder,
                 list.files(file.path(thisProjectPath,tempFolder))),
      recurse=TRUE)
  
  # clean up to avoid data garbage
  unlink(file.path(thisProjectPath,tempFolder), recursive = TRUE)
  
  print(paste("done with file:",myList[run]))
#}