\name{check_bridles_length}
\alias{check_bridles_length}
\title{
check of bridles length correctness
}
\description{
The function performs consistency checks of the values in the "BRIDLES_LENGTH" field of the hauls data table (TA).
}
\usage{
check_bridles_length(DataTA, wd, suffix)
}
\arguments{
    \item{DataTA}{Haul data table according to MEDITS protocol (TA)}
    \item{wd}{working directory path defined by the user}
    \item{suffix}{Suffix string of the Logfile}
}
\details{
The field BRIDLES_LENGTH can assume value 100 between 10-200 m of depth or 150 between 200-800 m.
}
\value{
The function returns always TRUE, because the outcome of the function is a warning that does not lock the execution of the RoME checks. The presence of inconsistencies in the data is reported in the logfile stored in the "Logfiles"" subdirectory of the "wd"" user-defined directory.
}
\references{Anonymus. 2017. MEDITS-Handbook. Version n. 9. MEDITS Working Group. 106 pp.https://www.sibm.it/MEDITS 2011/principaledownload.htm}
\author{I. Bitetto, W. Zupa}
\examples{
library(MEDITS)
wd <- tempdir()
suffix <- "27-02-2020 18:30"
check_bridles_length(MEDITS::TA, wd, suffix)
}
\keyword{error}
