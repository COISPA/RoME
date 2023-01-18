\name{check_consistencyTA_duration}
\alias{check_consistencyTA_duration}
\title{Consistency check of hauls duration in TA}
\description{The function checks whether the durations reported in the haul data (TA) are consistent with the differences between HAULING_TIME and SHOOTING_TIME.}
\usage{
check_consistencyTA_duration(DataTA, year, wd, suffix)
}
\arguments{
    \item{DataTA}{Haul data table according to MEDITS protocol (TA)}
    \item{year}{reference year for the analysis}
    \item{wd}{working directory path defined by the user}
    \item{suffix}{Suffix string of the Logfile}
}
\details{The DURATION, SHOOTING_TIME and HAULING_TIME fields have to be consistent}
\value{The function returns a boolean value. It is FALSE in case one or more durations in the TA table are not consistent with the differences between HAULING_TIME and SHOOTING_TIME.}
\references{Anonymus. 2017. MEDITS-Handbook. Version n. 9. MEDITS Working Group. 106 pp.https://www.sibm.it/MEDITS 2011/principaledownload.htm}
\author{I. Bitetto, W. Zupa}
\examples{
wd <- tempdir()
suffix="2020-03-05_time_h17m44s55"
check_consistencyTA_duration(RoME::TA,year=2012,wd,suffix)
}
\keyword{error}
