\name{calcPivotPoint}
\alias{calcPivotPoint}
\title{
Calculate pivot points
}
\description{
Calculate pivot points given the data of previou day
}
\usage{
calcPivotPoint(winddata)
}

\arguments{
  \item{winddata}{
The data must have the wind data format
}
}
\details{
See join data for more
}
\value{

  \item{S1 }{S1 line}
  \item{S2 }{S2 line}
  \item{S3 }{S3 line}
  \item{S4 }{S4 line}
  \item{P1 }{P1 line}
  \item{P2 }{P2 line}
  \item{P3 }{P3 line}
  \item{P4 }{P4 line}
  \item{PP }{PP line}
}
\references{
Trading stocks with Camarilla pivots
}
\author{
Qi Wang
}
\note{

}



\seealso{
\link{http://www.futuresmag.com/2012/10/24/trading-stocks-camarilla-pivots}
}
\examples{
function (winddata) 
{
    high <- max(winddata$high)
    low <- min(winddata$high)
    close <- winddata[nrow(winddata), "close"]
    RANGE <- high - low
    R4 = close + RANGE * 1.1/2
    R3 = close + RANGE * 1.1/4
    R2 = close + RANGE * 1.1/6
    R1 = close + RANGE * 1.1/12
    PP = (high + low + close)/3
    S1 = close - RANGE * 1.1/12
    S2 = close - RANGE * 1.1/6
    S3 = close - RANGE * 1.1/4
    S4 = close - RANGE * 1.1/2
    retPoints <- sort(c(PP, R1, R2, R3, R4, S1, S2, S3, S4))
    names(retPoints) <- c("S4", "S3", "S2", "S1", "PP", "R1", 
        "R2", "R3", "R4")
    return(retPoints)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~pivot }
