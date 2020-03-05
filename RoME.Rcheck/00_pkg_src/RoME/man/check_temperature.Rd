\name{check_temperature}
\alias{check_temperature}
\title{
Function to check the consistency of the temperature data stored in haul data table (TA).}
\description{
This function checks if the temperature by haul is in the range 10-30 Celsius degrees; moreover, a plot depth versus temperature is produced and stored in the Graph folder.}
\usage{
check_temperature(ResultDataTA,wd,suffix)
}
\arguments{
\item{ResultDataTA}{Haul data table according to MEDITS protocol (TA).
}
\item{wd}{Working directory selected by the user.
}
\item{suffix}{Suffix string of the Logfile.
}
}
\details{
This check uses the temperature range 10-30 Celsius degrees to provide quantitative warning and a plot, automatically stored in Graphs, for a qualitative inspection  of tha temperature data respectto depth. }
\value{
The function returns always TRUE, because the outcome of the function is a warning that does not lock the execution of the RoME checks. The presence of inconsistencies in the data is reported in the logfile stored in the "Logfiles" subdirectory of the "wd" user-defined directory.
}
\references{
Anonymus. 2017. MEDITS-Handbook. Version n. 9. MEDITS Working Group. 106 pp. http://www.sibm.it/MEDITS 2011/principaledownload.htm
}
\author{
I. Bitetto, W. Zupa
}
\examples{
library(MEDITS)
library(RoME)
wd=tempdir()
suffix= "27-02-2020 18:30"
check_temperature(TA,wd,suffix)
}
\keyword{temperature}
