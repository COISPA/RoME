\name{check_rubincode}
\alias{check_rubincode}
\title{
Function checking the correctness of species MEDITS code and faunistic category according to TM reference list}
\description{
The TM list contained in the INSTRUCTION MANUAL VERSION 9 MEDITS 2017 is taken as reference to check the correctness of species code and category.The function is applied to catch data table (TB), Biological data table (TC) and Individual data table (TE).}
\usage{
check_rubincode(ResultData,wd,suffix)
}
\arguments{
\item{ResultData}{alternatively: Catch data table (TB), Biological data table (TC) and Individual data table (TE).}
\item{wd}{Working directory selected by the user.}
\item{suffix}{Suffix string of the Logfile.}
}
\details{
The checks execution is stopped if any mismatching record was found.}
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
check_rubincode(TB,wd,suffix)
check_rubincode(TC,wd,suffix)
}
\keyword{rubincode,TM list}
