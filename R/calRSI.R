calRSI <-
function(data, period = 14){
  change <- c(0, diff(data$close))
  if(sum(is.na(change))){
    gain[is.na(change)] <- 0;
    warning("Removed all NA")
  }
  
  rsi <- gain <- loss <- change
  gain[change < 0] <- 0
  loss[change > 0] <- 0
  rsi[rsi != 0] <- 0
  
  .C("calcRSI", as.double(gain), as.double(loss),
     rsi_rsi = as.double(rsi), as.integer(period), as.integer(length(gain)))$rsi_rsi
}
