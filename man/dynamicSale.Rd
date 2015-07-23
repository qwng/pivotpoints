\name{dynamicSale}
\alias{dynamicSale}
\title{
Dynamic stop loss
}
\description{
Use a dynamic method to calculate the stop loss position
}
\usage{
dynamicSale(dailyData, inPosition, pivotPoints, shortOrLong = "short")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dailyData}{
The daily data in winddata format
}
  \item{inPosition}{
Buy in postion
}
  \item{pivotPoints}{
Calculate by function calcPivotPoint
}
  \item{shortOrLong}{
Whether go short or long
}
}
\details{
}
\value{
\item{salePois}{The stop loss position}
}
\references{
}
\author{
Qi Wang
}
\note{
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
}
\examples{

function (dailyData, inPosition, pivotPoints, shortOrLong = "short") 
{
    inDailyData <- dailyData[inPosition:nrow(dailyData), ]
    penetratePosition <- rbind(diff(inDailyData[, "close"] >= 
        pivotPoints["S3"]), diff(inDailyData[, "close"] >= pivotPoints["S2"]), 
        diff(inDailyData[, "close"] >= pivotPoints["S1"]), diff(inDailyData[, 
            "close"] >= pivotPoints["R1"]), diff(inDailyData[, 
            "close"] >= pivotPoints["R2"]), diff(inDailyData[, 
            "close"] >= pivotPoints["R3"]))
    penetratePosition_all <- colSums(penetratePosition)
    totalPen <- cumsum(penetratePosition_all)
    if (shortOrLong == "long") {
        maxInd <- 0
        for (j in 1:length(totalPen)) {
            if (maxInd < totalPen[j]) 
                maxInd <- totalPen[j]
            if ((maxInd - totalPen[j]) >= 2) 
                break
        }
        totalPenPois <- rep(0, length(totalPen))
        totalPenPois[j] <- 1
        salePoisCand <- which((totalPenPois + ((penetratePosition_all <= 
            -2) >= 1) + (inDailyData[-1, "close"] <= pivotPoints["S4"]) + 
            (inDailyData[-1, "close"] >= pivotPoints["R4"])) >= 
            1)
    }
    else {
        minInd <- 100
        for (j in 1:length(totalPen)) {
            if (minInd > totalPen[j]) 
                minInd <- totalPen[j]
            if ((totalPen[j] - minInd) >= 2) 
                break
        }
        totalPenPois <- rep(0, length(totalPen))
        totalPenPois[j] <- 1
        salePoisCand <- which((totalPenPois + ((penetratePosition_all >= 
            2) >= 1) + (inDailyData[-1, "close"] <= pivotPoints["S4"]) + 
            (inDailyData[-1, "close"] >= pivotPoints["R4"])) >= 
            1)
    }
    if (!length(salePoisCand)) 
        salePois <- nrow(dailyData)
    else salePois <- inPosition + salePoisCand[1]
    return(salePois)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~dynamic }
