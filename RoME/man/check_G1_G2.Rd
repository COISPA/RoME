\name{check_G1_G2}
\alias{check_G1_G2}
\title{Check of length measurements for G1 and G2 species}
\description{Check if for G1 and G2 species the length measurements are present in TC}
\usage{check_G1_G2(DataTC, wd, suffix)}
\arguments{
    \item{DataTC}{Biological data table according to MEDITS protocol (TC)}
    \item{wd}{working directory path defined by the user}
    \item{suffix}{Suffix string of the Logfile}
}
\details{
This check uses a new support table (list_g1_g2) containing the list of MEDITS G1 and G2 species and verify if the length has been collected for the selected species for each haul. If the length is lacking for any species in any haul, a warning message is given in the logfile.}
\value{The function returns always TRUE because the outcome of the function is a warning that does not lock the execution of the RoME checks. If the length is lacking for any species in any haul, a warning message is given in the logfile.}
\references{Anonymus. 2017. MEDITS-Handbook. Version n. 9. MEDITS Working Group. 106 pp.https://www.sibm.it/MEDITS 2011/principaledownload.htm}
\author{I. Bitetto, W. Zupa}
\examples{
library(RoME)
library(MEDITS)
wd <- tempdir()
suffix <- "27-02-2020 18:30"
check_G1_G2(MEDITS::TC,wd,suffix)
}
\keyword{ warning }
