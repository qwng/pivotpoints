calcPivotPoint <-
function(winddata){
  high <- max(winddata$high)
  low <- min(winddata$high)
  close <- winddata[nrow(winddata),"close"]
  #close <- (high + low) / 2
  RANGE <- high - low
  R4 = close + RANGE * 1.1/2
  R3 = close + RANGE * 1.1/4
  R2 = close + RANGE * 1.1/6
  R1 = close + RANGE * 1.1/12
  PP = (high + low + close) / 3
  S1 = close - RANGE * 1.1/12
  S2 = close - RANGE * 1.1/6
  S3 = close - RANGE * 1.1/4
  S4 = close - RANGE * 1.1/2 
  #if((S3 - S4) >= 20) S3 <-  S4 + 20
  #if((R4 - R3) >= 20) R3 <-  R4 - 20
  retPoints <- sort(c(PP, R1, R2, R3, R4, S1, S2, S3, S4))
  names(retPoints) <- c("S4", "S3", "S2", "S1", "PP", "R1", "R2", "R3", "R4")
  return(retPoints)
}
