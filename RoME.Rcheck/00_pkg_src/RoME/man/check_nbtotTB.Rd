\name{check_nbtotTB}
\alias{check_nbtotTB}
\title{Check total number of individuals in TB}
\description{The function checks that the total number of individuals is consistent with the sum of the individuals per sex}
\usage{check_nbtotTB(DataTB, wd, suffix)}
\arguments{
  \item{DataTB}{Catch data table according to MEDITS protocol (TB)}
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
    suffix="27-02-2020 18:30"
    DataTB = MEDITS::TB
    check_nbtotTB(DataTB, wd, suffix)
}
\keyword{ error }
