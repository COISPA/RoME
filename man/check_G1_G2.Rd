\name{check_G1_G2}
\alias{check_G1_G2}
\title{Check of length measurements for G1 and G2 species}
\description{Check if for G1 and G2 species the length measurements are present in TC}
\usage{check_G1_G2(DataTC, year, wd, suffix)}
\arguments{
    \item{DataTC}{Biological data table according to MEDITS protocol (TC)}
    \item{year}{reference year for the analysis}
    \item{wd}{working directory path defined by the user}
    \item{suffix}{Suffix string of the Logfile}
}
\details{
This check uses a new support table (list_g1_g2) containing the list of MEDITS G1 and G2 species and verify if the length has been collected for the selected species for each haul. If the length is lacking for any species in any haul, a warning message is given in the logfile.}
\value{The function returns always TRUE because the outcome of the function is a warning that does not block the execution of the 'RoME' checks. If the length is lacking for any species in any haul, a warning message is given in the logfile.}
\references{Anonymus. 2017. MEDITS-Handbook. Version n. 9. MEDITS Working Group. 106 pp.https://www.sibm.it/MEDITS 2011/principaledownload.htm}
\author{I. Bitetto, W. Zupa}
\examples{
wd <- tempdir()
suffix="2020-03-05_time_h17m44s55"
check_G1_G2(RoME::TC,year=2007,wd,suffix)
}
\keyword{ warning }
