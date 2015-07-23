join_data <-
function(folderPath = getwd(), rsiPeriod = 14){
library(readr)    
library(dplyr)
  fileList <- list.files(folderPath)
  
  # if there is a pre existing dataset in current workspace,
  # remove it
  
  if (exists("dataset")){
    rm(dataset)
  }
  
# if(file.exists(file.path(folderPath,"dataCombined.csv"))){
#    if(file.remove(file.path(folderPath,"dataCombined.csv"))) 
#      print("remove existed dataCombined.csv")
#  }
  
  for(file in fileList){
    # if the merged dataset doesn't exist, create it
    if (!exists("dataset")){
      dataset <- read_csv(file.path(folderPath,file), col_names = FALSE)
      if(ncol(dataset) > 7){
        dataset <- read_csv(file.path(folderPath,file), col_names = FALSE, skip = 1)
        dataset <- dataset[ ,1:7]
      }
    }
    
    # if the merged dataset does exist, append to it
    if (exists("dataset")){
      temp_dataset <- read_csv(file.path(folderPath,file), col_names = FALSE)
      if(ncol(temp_dataset) > 7) {
        temp_dataset <- read_csv(file.path(folderPath,file), col_names = FALSE, skip = 1)
        temp_dataset <- temp_dataset[ ,1:7]
      }
      dataset <- rbind(dataset, temp_dataset)
      rm(temp_dataset)
      print(file)
    }
  }
  
  dataset <- data.frame(dataset)
  DATETIME <- as.POSIXlt(as.character(dataset[,1]), format = "%Y/%m/%d %H:%M:%S")
  dataset <- data.frame(DATETIME, dataset[,3:5])
  colnames(dataset) <- c("DATETIME", "high", "low", "close")
  rsi <- calRSI(data = dataset, rsiPeriod)
  dataset <- data.frame(dataset, rsi_rsi = rsi)
  dataset <- filter(dataset, !duplicated(DATETIME))  
  write.csv(file = file.path(folderPath, "dataCombined.csv"),
  dataset, row.names = FALSE)
}
