\name{check_hauls_TLTA}
\alias{check_hauls_TLTA}
\title{Check presence of TL hauls in TA}
\description{
Check if the hauls in TL are present in TA}
\usage{
check_hauls_TLTA(DataTA, DataTL,wd, suffix)
}
\arguments{
  \item{DataTA}{Haul data table according to MEDITS protocol (TA)}
  \item{DataTL}{Litter data table according to MEDITS protocol (TL)}
  \item{wd}{working directory path defined by the user}
  \item{suffix}{Suffix string of the Logfile}
}
\details{The function checks whether all the hauls peresent in litter data table (TL) are included in the haul data table (TA). }
\value{The function returns TRUE if no error occurs, while FALSE is returned when there are missing hauls in the TA table.}
\references{Anonymus. 2017. MEDITS-Handbook. Version n. 9. MEDITS Working Group. 106 pp.https://www.sibm.it/MEDITS 2011/principaledownload.htm}
\author{I. Bitetto, W. Zupa}
\examples{
\donttest{
    library(RoME)
    wd <- tempdir()
    suffix="27-02-2020 18:30"
    DataTA = MEDITS::TA
    DataTL = TL_table
    check_hauls_TLTA<-function(DataTA,DataTL,wd,suffix)
}
}
\keyword{ error }
