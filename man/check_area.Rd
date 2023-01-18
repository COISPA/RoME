\name{check_area}
\alias{check_area}
\title{
 Check if TX files have the same area
}
\description{
 The function works with data of a single year of survey and checks if TX files have the same area code.}
\usage{
check_area(DataTA, DataTB, DataTC, DataTE=NA, DataTL=NA, year, wd, suffix)
}
\arguments{
  \item{DataTA}{Haul data table according to MEDITS protocol (TA)}
  \item{DataTB}{Catch data table according to MEDITS protocol (TB)}
  \item{DataTC}{Biological data table according to MEDITS protocol (TC)}
  \item{DataTE}{Individual biological data table according to MEDITS protocol (TE)}
  \item{DataTL}{Litter data table according to MEDITS protocol (TL)}
  \item{year}{reference year for the analysis}
  \item{wd}{working directory path defined by the user}
  \item{suffix}{Suffix string of the Logfile}
}
\details{
  TA, TB and TC tables are mandatory while TE and TL could be used where available.}
\value{
The function returns TRUE if no error occurs, while FALSE is returned when there are differences in the AREA code among the TX tables.
}
\references{Anonymus. 2017. MEDITS-Handbook. Version n. 9. MEDITS Working Group. 106 pp.https://www.sibm.it/MEDITS 2011/principaledownload.htm}
\author{I. Bitetto, W. Zupa}
\examples{
    wd <- tempdir()
    suffix="2020-03-05_time_h17m44s55"
    DataTA = RoME::TA
    DataTB = RoME::TB
    DataTC = RoME::TC
    DataTE = RoME::TE
    DataTL = RoME::TL
    check_area(DataTA, DataTB,DataTC,DataTE=NA,DataTL=NA,year=2012, wd, suffix)
}

\keyword{error}
