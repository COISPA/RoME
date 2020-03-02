\name{check_stratum_code}
\alias{check_stratum_code}
\title{
Function to check the correct codification of the strata in haul data table (TA).}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
check_stratum_code(ResultDataTA,Stratification,wd,suffix)
}
\arguments{
\item{ResultDataTA}{Haul data table according to MEDITS protocol (TA).
}
\item{Stratification}{Stratification scheme according to MEDITS protocol.
}
\item{wd}{Working directory selected by the user.
}
\item{suffix}{Suffix string of the Logfile.
}
}
\details{
This function checks if the stratum code associated to each haul is consistent with the code reported in MEDITS manual and in the table Stratification scheme, corresponding to the associated depth range.
}
\value{
The function returns always TRUE, because the outcome of the function is a warning that doesnot lock the execution of the RoME checks. The presence of inconsistencies in the data is reported in the logfile stored in the "Logfiles" subdirectory of the "wd" user-defined directory.
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
check_stratum_code(TA,stratification_scheme,wd,suffix)
}
\keyword{stratum code}
