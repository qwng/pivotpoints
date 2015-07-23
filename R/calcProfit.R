calcProfit <-
function(winddata){
  
  DATETIME <- as.POSIXlt(as.character(winddata[,1]), format = "%Y-%m-%d %H:%M:%S")
  winddata <- data.frame(DATETIME, winddata[, -1])
  
  dates <- unique(strftime(winddata$DATETIME, format="%y-%m-%d"))
  action <- NULL
  
  for(i in 2:length(dates)){
      idx <- which(strftime(winddata$DATETIME, format="%y-%m-%d") == dates[i-1])
      pivotPoints <- calcPivotPoint(winddata[idx, ])
      idx <- which(strftime(winddata$DATETIME, format="%y-%m-%d") == dates[i])
      dailyData <- winddata[idx, ]
      action <- rbind(action, dailyAction(dailyData, pivotPoints))
      print(i)
  }
  
  colnames(action) <- c("goLong", "goShort", "earn", "salePosition")
  
  return(data.frame(date = dates[-1],action))
}
