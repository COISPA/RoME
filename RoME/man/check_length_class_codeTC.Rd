\name{check_length_class_codeTC}
\alias{check_length_class_codeTC}
\title{Consistency check of LENGTH_CLASS}
\description{The function checks the consistency of field LENGTH_CLASSES_CODE in TC}
\usage{check_length_class_codeTC(DataTC,Specieslist=RoME::TM_list,wd,suffix)}
%- maybe also 'usage' for other objects documented here.
\arguments{
    \item{DataTC}{Biological data table according to MEDITS protocol (TC)}
    \item{Specieslist}{Information related to target species as reported in the TM list}
    \item{wd}{working directory path defined by the user}
    \item{suffix}{Suffix string of the Logfile}
}
\details{
The function checks whether the LENGTH_CLASS_CODE by species are consistent with those reported in the Specieslist dataset. When Specieslist is NA the TM_list dataset (included in the package) is used by default.
}
\value{
If the LENGTH_CLASS_CODE in TC table (biological data table) are not consistent with CODLON field in Specieslist dataset (or TM_list if Specieslist is NA) an error is returned. In case a LENGTH_CLASS_CODE is not reported for the given species no check is done and the function returns a warning message.
}
\references{Anonymus. 2017. MEDITS-Handbook. Version n. 9. MEDITS Working Group. 106 pp. http://www.sibm.it/MEDITS 2011/principaledownload.htm}
\author{I. Bitetto, W. Zupa}
\examples{
\dontrun{
library(MEDITS)
library(RoME)
DataTC <- MEDITS::TC
wd=tempdir()
suffix= "2020-03-05_time_h17m44s55"
check_length_class_codeTC(DataTC,Specieslist=NA,wd,suffix)
}
}
\keyword{error}
\keyword{warning}
