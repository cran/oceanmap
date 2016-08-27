\name{check_ts}
\alias{check_ts}
\title{
checks if daily \code{'.gz'}-file time series is complete}
\description{
checks if daily \code{'.gz'}-file time series in the present working directory is complete.}
\usage{
check_ts(sstring="*.gz",output=F)
}
\arguments{
  \item{sstring}{
Character string indicating search criteria for gz-files (default is \code{'*.gz'}). 
}
  \item{output}{
  weather the missing dates should be returned as vector (default is \code{F}).
}
}
\value{
optional vector of missing dates (see \code{output} argument).
}
\seealso{\link{name_split}, \link{check_gzfiles}} 


\author{
Robert K. Bauer
}

\examples{
## Example 1: plot '.gz'-files, following default plot-procedure
owd <- getwd()
setwd(system.file("test_files", package="oceanmap"))
check_gzfiles() # return file summary-table per filetype

gz.files <- Sys.glob('*.gz') # load sample-'.gz'-files
name_split(gz.files) # return summary-table per file
}
