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
Description of the error is reported in the logfile and/or in the console.
}
\references{Anonymus. 2017. MEDITS-Handbook. Version n. 9. MEDITS Working Group. 106 pp.https://www.sibm.it/MEDITS 2011/principaledownload.htm}
\author{I. Bitetto, W. Zupa}
\examples{
\dontrun{
printError(funname,check_without_errors, stop_)
}
}
\keyword{error}
