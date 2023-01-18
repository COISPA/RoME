\name{check_hauls_TBTA}
\alias{check_hauls_TBTA}
\title{Check of TB hauls in TA}
\description{The function check the presence of the TB (catch data table) hauls in the TA (haul data table)}
\usage{check_hauls_TBTA(DataTA, DataTB, year, wd, suffix)}
\arguments{
  \item{DataTA}{Haul data table according to MEDITS protocol (TA)}
  \item{DataTB}{Catch data table according to MEDITS protocol (TB)}
  \item{year}{reference year for the analysis}
  \item{wd}{working directory path defined by the user}
  \item{suffix}{Suffix string of the Logfile}
}
\details{The function check the presence of the TB (catch data table) hauls in the TA (haul data table)}
\value{The function returns TRUE if no error occurs, while FALSE is returned when an inconsistency is detected between haul and catch tables.}
\references{Anonymus. 2017. MEDITS-Handbook. Version n. 9. MEDITS Working Group. 106 pp.https://www.sibm.it/MEDITS 2011/principaledownload.htm}
\author{I. Bitetto, W. Zupa}
\examples{
\donttest{
    wd <- tempdir()
    suffix="2020-03-05_time_h17m44s55"
    DataTA <- RoME::TA
    DataTB <- RoME::TB
    check_hauls_TBTA(DataTA,DataTB,year=2008,wd,suffix)
    }
}
\keyword{ error }
