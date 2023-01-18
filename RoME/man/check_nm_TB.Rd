\name{check_nm_TB}
\alias{check_nm_TB}
\title{Check of consistency in number per sex set "not mandatory" in TB}
\description{Check if in TB there are the total number, number of females, males and undetermined for species G1}
\usage{
check_nm_TB(DataTB, year, wd, suffix)
}
\arguments{
 \item{DataTB}{Catch data table according to MEDITS protocol (TB)}
 \item{year}{reference year for the analysis}
 \item{wd}{working directory path defined by the user}
 \item{suffix}{Suffix string of the Logfile}
}
\details{
for the species G1 are not allowed that the fields related to total number, number of females, number of males and number of undetermined are simultaneously null, according to MEDITS manual version 9 of 2017.
}
\value{
The function returns TRUE if no error occurs, FALSE if one or more inconsistences in the individuals number per sex in TB is detected.
}
\references{Anonymus. 2017. MEDITS-Handbook. Version n. 9. MEDITS Working Group. 106 pp.https://www.sibm.it/MEDITS 2011/principaledownload.htm}
\author{I. Bitetto, W. Zupa}
\examples{
    wd <- tempdir()
    suffix="2020-03-05_time_h17m44s55"
    DataTB = RoME::TB
    year=2007
    check_nm_TB(DataTB, year, wd, suffix)
}
\keyword{ error }
