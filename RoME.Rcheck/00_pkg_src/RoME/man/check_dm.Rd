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
\value{The function returns a boolean value. It is FALSE in case wing and vertical opening values are out of the allowed ranges}
\references{Anonymus. 2017. MEDITS-Handbook. Version n. 9. MEDITS Working Group. 106 pp.https://www.sibm.it/MEDITS 2011/principaledownload.htm}
\author{I. Bitetto, W. Zupa}
\examples{
library(MEDITS)
wd <- tempdir()
suffix <- "27-02-2020 18:30"
check_dm(MEDITS::TA,wd,suffix)
}
\keyword{ error }
