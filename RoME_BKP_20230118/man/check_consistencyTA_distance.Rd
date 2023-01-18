\name{check_consistencyTA_distance}
\alias{check_consistencyTA_distance}
\title{Consistency check of distance in TA}
\description{The function checks whether the distances reported in the haul data (TA) are consistent with the hauls duration.}
\usage{
check_consistencyTA_distance(DataTA, wd, suffix)
}
\arguments{
    \item{DataTA}{Haul data table according to MEDITS protocol (TA)}
    \item{wd}{working directory path defined by the user}
    \item{suffix}{Suffix string of the Logfile}
}
\details{
Check between duration of the haul and distance (tolerance of 15%)
}
\value{
The function returns a boolean value. It is FALSE in case one or more distances in the TA table are out the 15\% of the tollerance from the expected values.
}
\references{Anonymus. 2017. MEDITS-Handbook. Version n. 9. MEDITS Working Group. 106 pp.https://www.sibm.it/MEDITS 2011/principaledownload.htm}
\author{I. Bitetto, W. Zupa}
\examples{
library(MEDITS)
wd=tempdir()
suffix="2020-03-05_time_h17m44s55"
library(RoME)
check_consistencyTA_distance(MEDITS::TA,wd,suffix)
}
\keyword{error}
