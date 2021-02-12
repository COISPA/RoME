\name{check_species_TBTC}
\alias{check_species_TBTC}
\title{
Function checking if all the target species in the catch data table (TB) are in Biological data table (TC)
}

\description{
This function verifies the presence of the target species (that is a subset of the all the species caught, reported in TB), in the TC table, where additional information (apart from number and weight) are collected.}
\usage{
check_species_TBTC(ResultTB,ResultTC,DataSpecies,wd,suffix)
}
\arguments{
\item{ResultTB}{Catch data table(TB).}
\item{ResultTC}{Biological data table (TC).}
\item{DataSpecies}{Information related to target species.}
\item{wd}{Working directory selected by the user.}
\item{suffix}{Suffix string of the Logfile.}
}
\details{
This function gives a warning message, thus the execution is not stopped when some target species are lacking in TC; the user is informed in the Logfile.
}
\value{
The function returns always TRUE, because the outcome of the function is a warning that does not lock the execution of the 'RoME' checks.
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
suffix= "2020-03-05_time_h17m44s55"
check_species_TBTC(TB,TC,DataTargetSpecies,wd,suffix)
}
\keyword{cross-check TBTC}
