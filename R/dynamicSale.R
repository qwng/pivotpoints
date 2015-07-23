dynamicSale <-
function(dailyData, inPosition, pivotPoints, shortOrLong = "short"){
  #Use dynamic method to find the sale point
  
  inDailyData <- dailyData[inPosition:nrow(dailyData), ]
  penetratePosition<- rbind(
    diff(inDailyData[,"close"] >= pivotPoints["S3"]),
    diff(inDailyData[,"close"] >= pivotPoints["S2"]),
    diff(inDailyData[,"close"] >= pivotPoints["S1"]),
    # diff(inDailyData[,"close"] >= pivotPoints["PP"]),
    diff(inDailyData[,"close"] >= pivotPoints["R1"]),
    diff(inDailyData[,"close"] >= pivotPoints["R2"]),
    diff(inDailyData[,"close"] >= pivotPoints["R3"]))
  
  penetratePosition_all <- colSums(penetratePosition)
  totalPen <- cumsum(penetratePosition_all)
  
  if(shortOrLong == "long"){
    maxInd <- 0
    for(j in 1:length(totalPen)){
      if(maxInd < totalPen[j]) maxInd <- totalPen[j]
      if((maxInd - totalPen[j]) >= 2) break
    }
    
    totalPenPois <- rep(0, length(totalPen))
    totalPenPois[j] <- 1
    
    salePoisCand <- which((totalPenPois + 
                             ((penetratePosition_all <= -2) >= 1) + 
                             (inDailyData[-1,"close"] <= pivotPoints["S4"]) + 
                             (inDailyData[-1,"close"] >= pivotPoints["R4"])) >= 1)
    
  } else{
    minInd <- 100
    for(j in 1:length(totalPen)){
      if(minInd > totalPen[j]) minInd <- totalPen[j]
      if((totalPen[j] - minInd) >= 2) break
    }
    
    totalPenPois <- rep(0, length(totalPen))
    totalPenPois[j] <- 1
    
    salePoisCand <- which((totalPenPois + 
                             ((penetratePosition_all >= 2) >= 1) + 
                             (inDailyData[-1,"close"] <= pivotPoints["S4"]) + 
                             (inDailyData[-1,"close"] >= pivotPoints["R4"])) >= 1)
  }
  
  if(!length(salePoisCand)) salePois <- nrow(dailyData) else
    salePois <- inPosition + salePoisCand[1]
  
  return(salePois)
}
