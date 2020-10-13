###################################################################
# editing Annotation spreadsheet
##################################################################
#from old script
setwd(dirname(sys.frame(1)$ofile))
library(readxl)
gropinDB <- read_excel("GroPIN-ver.3.xlsm", sheet = "Nonlinear")
run <- 1

# add to script
# mandatory fields
MetaData <- read_excel("ModelAnnotationExceltemplateV1.04.xlsx", sheet = "Generic Metadata Schema")

#Name of the Model
MetaData$Data[1] <-paste("Gropin Model Nr.",gropinDB$Microorganism)
#Identifier
MetaData$Data[3] <-paste0("gropin",gropinDB$Model,gropinDB$Microorganism)
MetaData$Data[8] <-"Value"
MetaData$Data[26] <-"R 3"



# parameters
MetaData$...12[132] <- "mydummypar"
MetaData$...13[132] <- "Input"
MetaData$...14[132] <- "My Dummy name"
MetaData$...15[132] <- "my dummy description"
MetaData$...16[132] <- "[]"
MetaData$...17[132] <- "double"
MetaData$...18[132] <- "double"
MetaData$...19[132] <- "my source"
MetaData$...20[132] <- "my subject"
MetaData$...21[132] <- "mydist"
MetaData$...22[132] <- "1.0"

write_xlsx(list("Generic Metadata Schema"=MetaData),path="test.xlsx")
