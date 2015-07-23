\name{dailyAction}
\alias{dailyAction}
\title{
Calculate action of one day
}
\description{
Calculate action of one day given daily data and pivot points
}
\usage{
dailyAction(dailyData, pivotPoints)
}
\arguments{
  \item{dailyData}{
The daily data in winddata format
}
  \item{pivotPoints}{
Calculate by function calcPivotPoint
}
}
\details{
}
\value{
\item{goLong}{The long position of one day}
\item{goShort}{The short position of one day}
\item{earn}{Earning of one day}
\item{salePosition}{The sale position of one day} 
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
function (dailyData, pivotPoints) 
{
    goLong <- 0
    goShort <- 0
    salePosition <- 0
    obs <- nrow(dailyData)
    penetrate20 <- (c(0, diff(dailyData$rsi_rsi >= 20)) == 1)
    penetrate80 <- (c(0, diff(dailyData$rsi_rsi <= 80)) == 1)
    lastMinutes <- (dailyData$DATETIME < (dailyData$DATETIME[obs] - 
        (dailyData$DATETIME[obs] - dailyData$DATETIME[obs - 1]) * 
            60))
    longPosition <- which((dailyData$close < pivotPoints["S3"]) * 
        (dailyData$close > pivotPoints["S4"]) * penetrate20 * 
        lastMinutes == 1)
    shortPosition <- which((dailyData$close > pivotPoints["R3"]) * 
        (dailyData$close < pivotPoints["R4"]) * penetrate80 * 
        lastMinutes == 1)
    if (!length(shortPosition) & !length(longPosition)) {
        earn <- 0
    }
    else if (!length(longPosition) & length(shortPosition)) {
        goShort <- shortPosition[1]
        salePosition <- dynamicSale(dailyData, goShort, pivotPoints, 
            shortOrLong = "short")
        earn <- dailyData[goShort, "close"] - dailyData[salePosition, 
            "close"]
    }
    else if (length(longPosition) & !length(shortPosition)) {
        goLong <- longPosition[1]
        salePosition <- dynamicSale(dailyData, goLong, pivotPoints, 
            shortOrLong = "long")
        earn <- dailyData[salePosition, "close"] - dailyData[goLong, 
            "close"]
    }
    else {
        if (shortPosition[1] < longPosition[1]) {
            goShort <- shortPosition[1]
            salePosition <- dynamicSale(dailyData, goShort, pivotPoints, 
                shortOrLong = "short")
            earn <- dailyData[goShort, "close"] - dailyData[salePosition, 
                "close"]
        }
        else {
            goLong <- longPosition[1]
            salePosition <- dynamicSale(dailyData, goLong, pivotPoints, 
                shortOrLong = "long")
            earn <- dailyData[salePosition, "close"] - dailyData[goLong, 
                "close"]
        }
    }
    return(c(goLong, goShort, earn, salePosition))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~action }
