\name{check_dm}
\alias{check_dm}
\title{
Check of "WING_OPENING" and "VERTICAL_OPENING" fields
}
\description{
The function checks the values in "WING_OPENING" and "VERTICAL_OPENING" field are in the allowed ranges (see INSTRUCTION MANUAL VERSION 9 MEDITS 2017).
}
\usage{
check_dm(DataTA, wd, suffix)
}
\arguments{
    \item{DataTA}{Haul data table according to MEDITS protocol (TA)}
    \item{wd}{working directory path defined by the user}
    \item{suffix}{Suffix string of the Logfile}
}
\details{The value ranges defined in the INSTRUCTION MANUAL VERSION 9 MEDITS (2017) for wing and vertical opening expressed in dm are respectively 50 - 250 and 10 - 100.}
\value{The function returns an error in case wing values are out of the allowed ranges, while it returns warnings in case vertical opening values are out of the allowed ranges and in case wing opening and vertical opening values are not integer numbers.}
\references{Anonymus. 2017. MEDITS-Handbook. Version n. 9. MEDITS Working Group. 106 pp.https://www.sibm.it/MEDITS 2011/principaledownload.htm}
\author{I. Bitetto, W. Zupa}
\examples{

suffix="2020-12-16_time_h10m52s55"
check_dm(MEDITS::TA,wd=tempdir(),suffix)

}
\keyword{ error }
