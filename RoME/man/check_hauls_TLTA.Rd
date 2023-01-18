\name{check_hauls_TLTA}
\alias{check_hauls_TLTA}
\title{Check presence of TL hauls in TA}
\description{
Check if the hauls in TL are present in TA}
\usage{
check_hauls_TLTA(DataTA,DataTL,year,wd,suffix)
}
\arguments{
  \item{DataTA}{Haul data table according to MEDITS protocol (TA)}
  \item{DataTL}{Litter data table according to MEDITS protocol (TL)}
  \item{year}{reference year for the analysis}
  \item{wd}{working directory path defined by the user}
  \item{suffix}{Suffix string of the Logfile}
}
\details{The function checks whether all the hauls peresent in litter data table (TL) are included in the haul data table (TA). }
\value{The function returns TRUE if no error occurs, while FALSE is returned when there are missing hauls in the TA table.}
\references{Anonymus. 2017. MEDITS-Handbook. Version n. 9. MEDITS Working Group. 106 pp.https://www.sibm.it/MEDITS 2011/principaledownload.htm}
\author{I. Bitetto, W. Zupa}
\examples{
\donttest{
    wd <- tempdir()
    suffix="2020-03-05_time_h17m44s55"
    DataTA = RoME::TA
    DataTL = RoME::TL
    year=2008
    check_hauls_TLTA(DataTA,DataTL,year,wd,suffix)
}
}
\keyword{ error }
