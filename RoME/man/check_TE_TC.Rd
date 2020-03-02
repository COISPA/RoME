\name{check_TE_TC}
\alias{check_TE_TC}
\title{
Function to verify the onsistency between individual data table (TE) and biological data table (TC) respect to numbe rof individuals.}
\description{
Check if the individuals by species, length, sex and maturity stage reported in TE are less than the number reported in TC}
\usage{
check_TE_TC(ResultDataTC,ResultDataTE,wd,suffix)
}
\arguments{
\item{ResultDataTC}{Biological data table(TC).}
\item{ResultDataTE}{Individual data table (TE).}
\item{wd}{Working directory selected by the user.}
\item{suffix}{Suffix string of the Logfile.}
}
\details{
This function gives an error message, thus the execution is  stopped if in TE are reported individuals not present in TC and if the number of individuals reported in TE is greater than the ones in TE; the user is informed in the Logfile.
}
\value{
The function returns TRUE if there is no error, while FALSE if there is one or more errors.The run, in case of error, thus, is stopped.
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
check_TE_TC(TC,ResultDataTE,wd,suffix)
}
\keyword{cross-check TETC}
