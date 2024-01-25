\name{check_haul_species_TCTB}
\alias{check_haul_species_TCTB}
\title{
Check species of TC in TB
}
\description{The function checks whether all the species present in TC (biological data table) must be listed in TB (catch data table)}
\usage{
check_haul_species_TCTB(DataTB, DataTC, year, wd, suffix)
}
\arguments{
    \item{DataTB}{chatch data table according to MEDITS protocol (TB)}
    \item{DataTC}{Biological data table according to MEDITS protocol (TC)}
    \item{year}{reference year for the analysis}
    \item{wd}{working directory path defined by the user}
    \item{suffix}{Suffix string of the Logfile}
}
\details{The function returns a worning message in the logfile.}
\value{
If a species present in the TC table (biological data table) is not reported in the TB (catch data table) an error message is reported in the logfile and a "Critical_errors" file is saved in the working directory reporting details on the errors.
}
\references{Anonymus. 2017. MEDITS-Handbook. Version n. 9. MEDITS Working Group. 106 pp.https://www.sibm.it/MEDITS 2011/principaledownload.htm}
\author{I. Bitetto, W. Zupa}
\examples{
wd <- tempdir()
suffix="2020-03-05_time_h17m44s55"
year=2008
check_haul_species_TCTB(RoME::TB, RoME::TC, year, wd, suffix)
}
\keyword{ warning }
