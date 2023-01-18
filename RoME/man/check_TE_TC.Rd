\name{check_TE_TC}
\alias{check_TE_TC}
\title{
Function to verify the consistency between individual data table (TE) and biological data table (TC) respect to number of individuals.}
\description{
Check if the individuals by species, length, sex and maturity stage reported in TE are less than the number reported in TC}
\usage{
check_TE_TC(ResultDataTC,ResultDataTE,year,wd,suffix)
}
\arguments{
\item{ResultDataTC}{Biological data table(TC).}
\item{ResultDataTE}{Individual data table (TE).}
\item{year}{reference year for the analysis.}
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
wd=tempdir()
suffix= "2020-03-05_time_h17m44s55"
tc <- RoME::TC
te <- RoME::TE
year=2012
check_TE_TC(tc,te,year,wd,suffix)
}
\keyword{cross-check TETC}
