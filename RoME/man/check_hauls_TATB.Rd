\name{check_hauls_TATB}
\alias{check_hauls_TATB}
\title{Check of TB hauls in TA}
\description{The function check the presence of the TB (catch data table) hauls in the TA (haul data table)}
\usage{check_hauls_TATB(DataTA,DataTB,wd,suffix)}
\arguments{
  \item{DataTA}{Haul data table according to MEDITS protocol (TA)}
  \item{DataTB}{Catch data table according to MEDITS protocol (TB)}
  \item{wd}{working directory path defined by the user}
  \item{suffix}{Suffix string of the Logfile}
}
\details{The function check the presence of the TB (catch data table) hauls in the TA (haul data table)}
\value{The function returns TRUE if no error occurs, while FALSE is returned when there are differences in the AREA code among the TX tables.}
\references{Anonymus. 2017. MEDITS-Handbook. Version n. 9. MEDITS Working Group. 106 pp.https://www.sibm.it/MEDITS 2011/principaledownload.htm}
\author{I. Bitetto, W. Zupa}
\examples{
    library(RoME)
    library(MEDITS)
    wd <- tempdir()
    suffix=paste(as.character(Sys.Date()),format(Sys.time(), "_time h%Hm%Ms%OS0"),sep="")
    DataTA <- MEDITS::TA
    DataTA <- DataTA[DataTA$YEAR ==2018 , ]
    DataTB <- MEDITS::TB
    DataTB <- DataTB[DataTB$YEAR ==2018 , ]
    check_hauls_TATB(DataTA,DataTB,wd,suffix)
}

\keyword{ error }
