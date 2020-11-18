\name{check_haul_species_TCTB}
\alias{check_haul_species_TCTB}
\title{
Check species of TC in TB
}
\description{The function checks whether all the species present in TC (biological data table) must be listed in TB (catch data table)}
\usage{
check_haul_species_TCTB(DataTB, DataTC, wd, suffix)
}
\arguments{
    \item{DataTB}{chatch data table according to MEDITS protocol (TB)}
    \item{DataTC}{Biological data table according to MEDITS protocol (TC)}
    \item{wd}{working directory path defined by the user}
    \item{suffix}{Suffix string of the Logfile}
}
\details{The function returns a worning message in the logfile.}
\value{
The function returns always TRUE because the outcome of the function is a warning that does not lock the execution of the RoME checks. If a species present in the TC table (biological data table) is not reported in the TB (catch data table) a warning message is given in the logfile.
}
\references{Anonymus. 2017. MEDITS-Handbook. Version n. 9. MEDITS Working Group. 106 pp.https://www.sibm.it/MEDITS 2011/principaledownload.htm}
\author{I. Bitetto, W. Zupa}
\examples{
library(RoME)
library(MEDITS)
wd <- tempdir()
suffix="2020-03-05_time_h17m44s55"
check_haul_species_TCTB(MEDITS::TB, MEDITS::TC, wd, suffix)
}
\keyword{ warning }
