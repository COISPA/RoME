\name{printError}
\alias{printError}
\title{
Management of the error in logfile.}
\description{
Management of the error in logfile.}

\usage{
printError(funname,check_without_errors, stop_)
}
\arguments{
\item{funname}{name of the check function.}
\item{check_without_errors}{TRUE if there is no error, FALSE if there is any error.}
\item{stop_}{TRUE if the 'RoME' function has to stop, FALSE if the run should continue}
}
\value{
The function updates the `stop_` value and returns it as a logical value.
}
\references{Anonymus. 2017. MEDITS-Handbook. Version n. 9. MEDITS Working Group. 106 pp. \url{https://www.sibm.it/MEDITS\%202011/principaledownload.htm}}
\author{I. Bitetto, W. Zupa}
\examples{
printError("check_abundance", FALSE, FALSE)
}
\keyword{error}


