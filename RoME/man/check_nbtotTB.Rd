\name{check_nbtotTB}
\alias{check_nbtotTB}
\title{Check total number of individuals in TB}
\description{The function checks that the total number of individuals is consistent with the sum of the individuals per sex}
\usage{check_nbtotTB(DataTB, year, wd, suffix)}
\arguments{
  \item{DataTB}{Catch data table according to MEDITS protocol (TB)}
  \item{year}{reference year for the analysis}
  \item{wd}{working directory path defined by the user}
  \item{suffix}{Suffix string of the Logfile}
}
\details{
The function checks that the total number of individuals is consistent with the sum of the individuals per sex
}
\value{
The function returns TRUE if no error occurs, FALSE if one or more inconsistences in the individuals number is detected.
}
\references{Anonymus. 2017. MEDITS-Handbook. Version n. 9. MEDITS Working Group. 106 pp.https://www.sibm.it/MEDITS 2011/principaledownload.htm}
\author{I. Bitetto, W. Zupa}
\examples{
    wd <- tempdir()
    suffix="2020-03-05_time_h17m44s55"
    DataTB = RoME::TB
    year=2007
    check_nbtotTB(DataTB,year, wd, suffix)
}
\keyword{ error }
