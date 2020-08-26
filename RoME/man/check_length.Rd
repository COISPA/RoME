\name{check_length}
\alias{check_length}
\title{Check of length classes in TC}
\description{The function checks the consistency of length classes in TC.}
\usage{check_length(DataTC,DataSpecies=NA,wd,suffix)}
\arguments{
    \item{DataTC}{Biological data table according to MEDITS protocol (TC)}
    \item{DataSpecies}{Information related to target species}
    \item{wd}{working directory path defined by the user}
    \item{suffix}{Suffix string of the Logfile}
}
\details{The function checks whether the length classes by species are included in the range reported in the DataSpecies dataset. When DataSpecies is NA the DataTargetSpecies dataset (included in the package) is used by default.}
\value{
The function returns TRUE (warning message in logfile) in case of LENGTH_CLASS value out of the allowed range. If the length classes in TC table (biological data table) are not consistent with DataSpecies dataset (or DataTargetSpecies if DataSpecies is NA) a warning message is given in the logfile. Furthermore, the function returns FALSE in case the value in the LENGTH_CLASS field is empty, exiting with an error message.
}
\references{Anonymus. 2017. MEDITS-Handbook. Version n. 9. MEDITS Working Group. 106 pp. http://www.sibm.it/MEDITS 2011/principaledownload.htm}
\author{I. Bitetto, W. Zupa}
\examples{
library(MEDITS)
library(RoME)
wd=tempdir()
DataTC <- RoME::TC[1:20,]
DataSpecies=NA
suffix= "27-02-2020 18:30"
check_length(DataTC,DataSpecies,wd,suffix)
}
\keyword{warning}
