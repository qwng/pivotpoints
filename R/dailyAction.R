dailyAction <-
function(dailyData, pivotPoints){
  goLong <- 0
  goShort <- 0
  salePosition <- 0
  obs <- nrow(dailyData)
  penetrate20 <- (c(0, diff(dailyData$rsi_rsi>=20)) == 1)
  penetrate80 <- (c(0, diff(dailyData$rsi_rsi<=80)) == 1)
  
  # Define last minutes vector, if it is less than 60 minutes to market close,
  # do nothing even if the buying condition satisfies
  lastMinutes <- (dailyData$DATETIME < (dailyData$DATETIME[obs]
                                        - (dailyData$DATETIME[obs] - dailyData$DATETIME[obs-1]) * 60))
  
  # Calculate long and short position
  
  longPosition <- which((dailyData$close < pivotPoints["S3"]) * 
                          (dailyData$close > pivotPoints["S4"]) * penetrate20 * lastMinutes == 1)
  
  shortPosition <- which((dailyData$close > pivotPoints["R3"]) * 
                           (dailyData$close < pivotPoints["R4"]) * penetrate80 * lastMinutes == 1)
  
  
  if(!length(shortPosition) & !length(longPosition)){
    # No actiojn is taken, earn are set to 0
    
    earn <- 0
    
    
  } else if(!length(longPosition) & length(shortPosition)){
    
    # Go short when there is no long position
    
    # Set short position
    goShort <- shortPosition[1]
    
    # Calculate the salePosition by function dynamicSale
    salePosition <- dynamicSale(dailyData, goShort, pivotPoints, shortOrLong = "short")
    
    # Calculate earning
    earn <- dailyData[goShort, "close"] - dailyData[salePosition, "close"]
    
  } else if(length(longPosition) & !length(shortPosition)){
    
    # Go long when there is no short position
    
    # Set long position
    goLong <- longPosition[1]
    
    # Calculate the salePosition by function dynamicSale
    salePosition <- dynamicSale(dailyData, goLong, pivotPoints, shortOrLong = "long")
    
    # Calculate earning
    earn <-  dailyData[salePosition, "close"] - dailyData[goLong, "close"]
    
  } else{
    
    # Compare when both of long and short are not empty
    
    if(shortPosition[1] < longPosition[1]){
      # Go short when it happens first
      
      # Set short position
      goShort <- shortPosition[1]
      
      # Calculate the salePosition by function dynamicSale
      salePosition <- dynamicSale(dailyData, goShort, pivotPoints, shortOrLong = "short")
      
      # Calculate earning
      earn <- dailyData[goShort, "close"] - dailyData[salePosition, "close"]
    } else{
      # Go long when it happens first
      
      # Set long position
      goLong <- longPosition[1]
      
      # Calculate the salePosition by function dynamicSale
      salePosition <- dynamicSale(dailyData, goLong, pivotPoints, shortOrLong = "long")
      
      # Calculate earning
      earn <-  dailyData[salePosition, "close"] - dailyData[goLong, "close"]
      
    }
  }
  
  return(c(goLong, goShort, earn, salePosition))  
}
