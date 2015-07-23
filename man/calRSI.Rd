\name{calRSI}
\alias{calRSI}
\title{
Calculate RSI
}
\description{
Calculate RSI for each dataset
}
\usage{
calRSI(data, period = 14)
}
\arguments{
  \item{data}{
  The data should have the winddata format without RSI
}
  \item{period}{
  The period used to calcualte RSI, default is 14
}
}
\details{

}
\value{
\item{RSI}{The RSI of the position}
}
\references{
Relative Strength Index (RSI)
}
\author{
Qi Wang
}
\note{
}


\seealso{
\link{http://stockcharts.com/school/doku.php?id=chart_school:technical_indicators:relative_strength_index_rsi}
\examples{
function (data, period = 14) 
{
    change <- c(0, diff(data$close))
    if (sum(is.na(change))) {
        gain[is.na(change)] <- 0
        warning("Removed all NA")
    }
    rsi <- gain <- loss <- change
    gain[change < 0] <- 0
    loss[change > 0] <- 0
    rsi[rsi != 0] <- 0
    dyn.load("./code/calcRSI.so")
    .C("calcRSI", as.double(gain), as.double(loss), rsi_rsi = as.double(rsi), 
        as.integer(period), as.integer(length(gain)))$rsi_rsi
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~RSI }

