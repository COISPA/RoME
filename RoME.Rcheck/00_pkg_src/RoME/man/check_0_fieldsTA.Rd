\name{check_0_fieldsTA}
\alias{Checks 0 fields in TA}
\title{Checks the presence of 0 fields in TA}
\description{
The function checks the presence of 0 fields in the following haul data table (TA, according to MEDITS protocol) fields: WING_OPENING, WARP_DIAMETER  and VERTICAL_OPENING}
\usage{check_0_fieldsTA(ResultDataTA,wd,suffix)}
\arguments{
    \item{ResultDataTA}{Haul data table according to MEDITS protocol (TA)}
    \item{wd}{working directory path defined by the user}
    \item{suffix}{Suffix string of the Logfile}
    }

\value{The function returns a boolean value. It is FALSE in case 0 values are detected in the TA table's fields}
\references{Anonymus. 2017. MEDITS-Handbook. Version n. 9. MEDITS Working Group. 106 pp.https://www.sibm.it/MEDITS 2011/principaledownload.htm}
\author{I. Bitetto, W. Zupa}
\examples{
library(MEDITS)
wd <- tempdir()
suffix <- "27-02-2020 18:30"
ResultDataTA = read.csv("~/GitHub/RoME/data/TA_GSA18_1994-2018.csv", sep=";")
}

\keyword{ error }

