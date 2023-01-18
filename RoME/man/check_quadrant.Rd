\name{check_quadrant}
\alias{check_quadrant}
\title{
Check start and end quadrant for each haul
}
\description{
Function checking that the shooting quadrant and the hauling quadrant are the same.
}
\usage{
check_quadrant(ResultDataTA,year,wd,suffix)
}
\arguments{
\item{ResultDataTA}{Haul data table according to MEDITS protocol (TA).}
\item{year}{reference year for the analysis}
\item{wd}{Working directory selected by the user.}
\item{suffix}{Suffix string of the Logfile.}
}
\details{
The function returns a warning if shooting and hauling quadrant are not the same.
}
\value{
The function returns always TRUE, because the outcome of the function is a warning that does not lock the execution of the 'RoME' checks. The presence of inconsistencies in the data is reported in the logfile stored in the "Logfiles" subdirectory of the "wd" user-defined directory.
}
\references{
For the definition of the quadrants, please refer to:
Anonymus. 2017. MEDITS-Handbook. Version n. 9. MEDITS Working Group. 106 pp. http://www.sibm.it/MEDITS 2011/principaledownload.htm
}
\author{
I. Bitetto, W. Zupa
}
\examples{
wd=tempdir()
suffix= "2020-03-05_time_h17m44s55"
TA <- RoME::TA
year=2007
check_quadrant(TA,year,wd,suffix)
}
\keyword{quadrant}
