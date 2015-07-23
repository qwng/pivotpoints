\name{calcProfit}
\alias{calcProfit}
\title{
Calculate earning 
}
\description{
Calculate earning for given strategy
}
\usage{
calcProfit(winddata)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{winddata}{
The data must have the wind data format
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
None
}
\author{
Qi Wang
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{


## The function is currently defined as
function (winddata) 
{
    dates <- unique(strftime(winddata$DATETIME, format = "\%y-\%m-\%d"))
    action <- NULL
    for (i in 2:length(dates)) {
        idx <- which(strftime(winddata$DATETIME, format = "\%y-\%m-\%d") == 
            dates[i - 1])
        pivotPoints <- calcPivotPoint(winddata[idx, ])
        idx <- which(strftime(winddata$DATETIME, format = "\%y-\%m-\%d") == 
            dates[i])
        dailyData <- winddata[idx, ]
        action <- rbind(action, dailyAction(dailyData, pivotPoints))
        print(i)
    }
    colnames(action) <- c("goLong", "goShort", "earn", "salePosition")
    return(data.frame(date = dates[-1], action))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~profit }

