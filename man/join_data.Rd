\name{join_data}
\alias{join_data}
\title{
Join separate files
}
\description{
Join separate .csv files into one file, calculate RSI and make it into winddata format
}
\usage{
join_data(folderPath = getwd(), rsiPeriod = 14)
}
\arguments{
  \item{folderPath}{
The path where the files located
}
  \item{rsiPeriod}{
The period to calculate RSI, default value 14
}
}
\details{
}
\value{
Output will be put into the same folder with the name dataCombined.csv
}
\references{
}
\author{
Qi Wang
}
\note{
}


\seealso{
}
\examples{

function (folderPath = getwd(), rsiPeriod = 14) 
{
    fileList <- list.files(folderPath)
    if (exists("dataset")) {
        rm(dataset)
    }
    for (file in fileList) {
        if (!exists("dataset")) {
            dataset <- read_csv(file.path(folderPath, file), 
                col_names = FALSE)
            if (ncol(dataset) > 7) {
                dataset <- read_csv(file.path(folderPath, file), 
                  col_names = FALSE, skip = 1)
                dataset <- dataset[, 1:7]
            }
        }
        if (exists("dataset")) {
            temp_dataset <- read_csv(file.path(folderPath, file), 
                col_names = FALSE)
            if (ncol(temp_dataset) > 7) {
                temp_dataset <- read_csv(file.path(folderPath, 
                  file), col_names = FALSE, skip = 1)
                temp_dataset <- temp_dataset[, 1:7]
            }
            dataset <- rbind(dataset, temp_dataset)
            rm(temp_dataset)
            print(file)
        }
    }
    dataset <- dataset[, c(1, 3:5)]
    colnames(dataset) <- c("DATETIME", "high", "low", "close")
    dataset[, 1] <- lapply(dataset[, 1], function(x) as.POSIXlt(as.character(x), 
        format = "\%Y/\%m/\%d \%H:\%M:\%S"))
    rsi <- calRSI(data = dataset, rsiPeriod)
    dataset <- data.frame(dataset, rsi_rsi = rsi)
    write.csv(file = file.path(folderPath, "dataCombined.csv"), 
        dataset, row.names = FALSE)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~join }
