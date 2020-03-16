\name{check_depth}
\alias{check_depth}
\title{Check between start depth and end depth}
\description{Check if that difference between start depth and end depth is not greater than 20\%}
\usage{check_depth(DataTA, wd, suffix)}
\arguments{
    \item{DataTA}{Haul data table according to MEDITS protocol (TA)}
    \item{wd}{working directory path defined by the user}
    \item{suffix}{Suffix string of the Logfile}
}
\details{The difference between start depth and end depth should be not greater than 20\%.}
\value{The function returns always TRUE because the outcome of the function is a warning that does not lock the execution of the RoME checks. The presence of inconsistencies between start depth and end depth is reported in the logfile stored in the "Logfiles"" subdirectory of the "wd"" user-defined directory.}
\references{Anonymus. 2017. MEDITS-Handbook. Version n. 9. MEDITS Working Group. 106 pp.https://www.sibm.it/MEDITS 2011/principaledownload.htm}
\author{I. Bitetto, W. Zupa}
\examples{
library(RoME)
library(MEDITS)
wd <- tempdir()
suffix="2020-03-05_time h17m44s55"
check_depth(MEDITS::TA,wd,suffix)
}

\keyword{warning}
