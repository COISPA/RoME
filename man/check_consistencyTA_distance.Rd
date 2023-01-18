\name{check_consistencyTA_distance}
\alias{check_consistencyTA_distance}
\title{Consistency check of distance in TA}
\description{The function checks whether the distances reported in the haul data (TA) are consistent with the hauls duration.}
\usage{
check_consistencyTA_distance(DataTA, year, wd, suffix)
}
\arguments{
    \item{DataTA}{Haul data table according to MEDITS protocol (TA)}
    \item{year}{reference year for the analysis}
    \item{wd}{working directory path defined by the user}
    \item{suffix}{Suffix string of the Logfile}
}
\details{
Check between duration of the haul and distance (tolerance of 15\%). The function does not check the presence of NA values in the DISTANCE field that are removed from the analysis. The eventual presence of empty records in the DISTANCE field is checked by the \code{check_no_empty_fields} function.
}
\value{
The function generates warning messages in the logfile and returns always TRUE.
}
\references{Anonymus. 2017. MEDITS-Handbook. Version n. 9. MEDITS Working Group. 106 pp.https://www.sibm.it/MEDITS 2011/principaledownload.htm}
\author{I. Bitetto, W. Zupa}
\examples{
wd=tempdir()
suffix="2020-03-05_time_h17m44s55"
check_consistencyTA_distance(RoME::TA,year=2012,wd,suffix)
}
\keyword{error}
