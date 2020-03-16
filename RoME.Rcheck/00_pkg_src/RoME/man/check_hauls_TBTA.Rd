\name{check_hauls_TBTA}
\alias{check_hauls_TBTA}
\title{Check of TB hauls in TA}
\description{The function check the presence of the TB (catch data table) hauls in the TA (haul data table)}
\usage{check_hauls_TBTA(DataTA, DataTB, wd, suffix)}
\arguments{
  \item{DataTA}{Haul data table according to MEDITS protocol (TA)}
  \item{DataTB}{Catch data table according to MEDITS protocol (TB)}
  \item{wd}{working directory path defined by the user}
  \item{suffix}{Suffix string of the Logfile}
}
\details{The function check the presence of the TB (catch data table) hauls in the TA (haul data table)}
\value{The function returns TRUE if no error occurs, while FALSE is returned when an inconsistency is detected between haul and catch tables.}
\references{Anonymus. 2017. MEDITS-Handbook. Version n. 9. MEDITS Working Group. 106 pp.https://www.sibm.it/MEDITS 2011/principaledownload.htm}
\author{I. Bitetto, W. Zupa}
\examples{
    library(RoME)
    library(MEDITS)
    wd <- tempdir()
    suffix="27-02-2020 18:30"
    DataTA <- MEDITS::TA
    DataTA <- DataTA[DataTA$YEAR ==2018 , ]
    DataTB <- MEDITS::TB
    DataTB <- DataTB[DataTB$YEAR ==2018 , ]
    check_hauls_TBTA(DataTA,DataTB,wd,suffix)
}
\keyword{ error }
