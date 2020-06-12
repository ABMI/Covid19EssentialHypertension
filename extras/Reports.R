dataFolder <- "" #where the original results are stored
exportFolder <- "" #where the figures/tables for the paper will be stored

resultsZipFiles <- c() #zip files of results
for(resultsZipFile in resultsZipFiles){
  Covid19EssentialHypertension::prepareForEvidenceExplorer(resultsZipFile = resultsZipFile, dataFolder = dataFolder)
}

databaseIds <- c("CUIMC","HIRA")

source(file.path("extras","DataPulls.R"))
source(file.path("extras","PlotsAndTables.R"))
source(file.path("extras","FuncitonsForReporting.R"))


#load files into the environment
for (i in seq(length(databaseIds))){
  databaseId = databaseIds[i]
  
  files <- list.files(dataFolder, pattern = sprintf("%s.rds", databaseId))
  files<-files[!grepl("tNA_cNA",files)]
  if (i==1){
    connection <- NULL
    positiveControlOutcome <- NULL
    
    splittableTables <- c("covariate_balance")#c("covariate_balance", "preference_score_dist", "kaplan_meier_dist")
    
    # Remove data already in global environment:
    tableNames <- gsub("(_t[0-9]+_c[0-9]+)|(_)[^_]*\\.rds", "", files) 
    camelCaseNames <- SqlRender::snakeCaseToCamelCase(tableNames)
    camelCaseNames <- unique(camelCaseNames)
    camelCaseNames <- camelCaseNames[!(camelCaseNames %in% SqlRender::snakeCaseToCamelCase(splittableTables))]
    rm(list = camelCaseNames)
    
    #load files
    lapply(files, loadFile)
  }else {
    lapply(files, loadFile)
  }
}

#load files into the environment
for (i in seq(length(databaseIds))){
  databaseId = databaseIds[i]
  dataFolder <- file.path(studyFolder,"shinyData")
  files <- list.files(dataFolder, pattern = sprintf("%s.rds", databaseId))
  files<-files[!grepl("tNA_cNA",files)]
  if (i==1){
    connection <- NULL
    positiveControlOutcome <- NULL
    
    splittableTables <- c("covariate_balance")#c("covariate_balance", "preference_score_dist", "kaplan_meier_dist")
    
    # Remove data already in global environment:
    tableNames <- gsub("(_t[0-9]+_c[0-9]+)|(_)[^_]*\\.rds", "", files) 
    camelCaseNames <- SqlRender::snakeCaseToCamelCase(tableNames)
    camelCaseNames <- unique(camelCaseNames)
    camelCaseNames <- camelCaseNames[!(camelCaseNames %in% SqlRender::snakeCaseToCamelCase(splittableTables))]
    rm(list = camelCaseNames)
    
    #load files
    lapply(files, loadFileRev)
  }else {
    lapply(files, loadFileRev)
  }
}


for(databaseId in databaseIds){
  balance<-getBalance(databaseId,
                      studyFolder,
                      targetId,
                      comparatorId,
                      primaryAnalysisId,
                      outcomeId)
  
  ##Table 1
  Table1 <- prepareTable1(balance,
                          beforeLabel = "Before matching",
                          afterLabel = "After matching",
                          targetLabel = targetName,
                          comparatorLabel = comparatorName,
                          percentDigits = 1,
                          stdDiffDigits = 2,
                          output = "latex",
                          pathToCsv = file.path("extras","Table1Specs.csv"))
  Table1[,1]<-as.character(Table1[,1])
  Table1[,1]<-gsub(" \\(Unknown unit\\)","",Table1[,1])
  
  Table1<-labFormmating(Table1,percentDigits=1)
  if(!file.exists(file.path(resultFolder,"Table1"))) dir.create(file.path(resultFolder,"Table1"))
  
  write.csv(Table1,file.path(resultFolder, "Table1",sprintf("Table1_%s_t%d_c%d_o%d_a%d.csv",
                                                            databaseId,
                                                            targetId,
                                                            comparatorId,
                                                            outcomeId,
                                                            primaryAnalysisId
  )))
  
}