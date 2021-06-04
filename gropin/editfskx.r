#library(rjson)
library(zip)
library(stringr)

# project path / obsolete in source mode
thisProjectPath <- "C:/Users/Joker/Documents/Projects/ModelRepository/gropin/"
tempFolder <- "temp/"
sourceSubFolder <- "fskx/"
targetSubFolder <- "finishedFSKX/"

myList <- list.files(file.path(thisProjectPath,sourceSubFolder),pattern="*.fskx")



if(!dir.exists(file.path(thisProjectPath,targetSubFolder))){
  dir.create(file.path(thisProjectPath,targetSubFolder))
}


# bring current date into format used by meta data schema
currentDate <- sub("0?(.+)-0?(.+)-0?(.+)", "\\1,\\2,\\3", Sys.Date())

# new metadata string
newCreationDate <- paste0(",\"creationDate\":[",
                          currentDate,
                          "],\"modificationDate\":[]")

#run <- 1
for(run in 1:length(myList)) {
  # check if folders exist
  if(!dir.exists(file.path(thisProjectPath,tempFolder))){
    dir.create(file.path(thisProjectPath,tempFolder))
  }
  
  zipSourceFile <- paste0(thisProjectPath,sourceSubFolder,myList[run])
  zipTargetFile <- paste0(thisProjectPath,targetSubFolder,myList[run])


  # unload fskx file into temp folder
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
  
  ##############################################################
  # ALL CHANGES HAVE TO BE DONE BY NOW
  ##############################################################
  
  
  #newData <- gsub(pattern = "Fuhrmann",replace = "Bloedmann",x = oldData)
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
}