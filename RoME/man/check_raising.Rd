\name{check_raising}
\alias{check_raising}
\title{
Function checking the correctness of species MEDITS code and faunistic category according to TM reference list}
\description{
The TM list contained in the INSTRUCTION MANUAL VERSION 9 MEDITS 2017 is taken as reference to check the correctness of species code and category.}
\usage{
check_raising(ResultDataTB,ResultDataTC,wd,suffix)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{ResultDataTB}{Catch data table (TB) .
}\item{ResultDataTC}{Biological data table (TC).
}
\item{wd}{Working directory selected by the user.
}
\item{suffix}{Suffix string of the Logfile.
}
}
\details{
The checks execution is stopped if any mismatching record was found.
}
\value{
The function returns TRUE if no error occurs, FALSE if some error record was found.
}
\references{
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
check_raising(ResultDataTB,ResultDataTC,wd,suffix)
}
\keyword{raising, sub-sampling}
