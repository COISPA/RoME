\name{check_stratum}
\alias{check_stratum}
\title{Function that checks the consistency between start and end depth according to the stratum.}
\description{
Start depth and end depth of each haul should be in the same stratum. }
\usage{
check_stratum(ResultData,wd,suffix)
}
\arguments{
\item{ResultData}{Haul data table according to MEDITS protocol (TA).
}
\item{wd}{Working directory selected by the user.
}
\item{suffix}{Suffix string of the Logfile.
}

}
\details{
Start depth and end depth of each haul should be in the same stratum. The strata are the ones defined according to the MEDITS protocol: 10-15 m; 50-100 m; 100-200 m; 200-500m; 500-800 m.}
\value{
The function returns always TRUE, because the outcome of the function is a warning that does not lock the execution of the RoME checks. The presence of inconsistencies in the data is reported in the logfile stored in the "Logfiles" subdirectory of the "wd" user-defined directory.
}
\references{
For the definition of the strata, please refer to:
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
check_stratum(TA,wd,suffix)
}
\keyword{stratum}
