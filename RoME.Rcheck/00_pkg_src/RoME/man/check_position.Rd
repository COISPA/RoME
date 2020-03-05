\name{check_position}
\alias{check_position}
\title{Plot of haul positions}
\description{The function generate three different plots, haul start position, haul end position and start and end positions together.}
\usage{check_position(DataTA, wd, suffix)}
\arguments{
    \item{DataTA}{Haul data table according to MEDITS protocol (TA)}
    \item{wd}{working directory path defined by the user}
    \item{suffix}{Suffix string of the Logfile}
}
\details{
The haul position maps are saved in the Graph directory allocated in the user defined wd directory.}
\value{The function generate three maps of the haul position that are stored in the Graph folder allocated in the user defined wd directory}
\references{Anonymus. 2017. MEDITS-Handbook. Version n. 9. MEDITS Working Group. 106 pp.https://www.sibm.it/MEDITS 2011/principaledownload.htm}
\author{I. Bitetto, W. Zupa}
\examples{
library(MEDITS)
wd <- tempdir()
suffix="2020-03-05_time h17m44s55"
check_position(MEDITS::TA,wd,suffix)
}

\keyword{ warning }
\keyword{ plot }
