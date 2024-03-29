\name{check_raising}
\alias{check_raising}
\title{
Function checking if, in case of sub-sampling in TC, the Total number and the number per sex in TB is raised correctly}
\description{
This function takes into account also the possibility of a "differentiated" sampling, according to the MEDITS protocol.
The word "Fraction" means any sub-group of individual from the total catch of a species (males, females, large sized individuals, small individuals, juveniles, etc.) on which it could be proceed to a sub-sample. For example: total weight = 1000 g which is divided into 100g of big individuals and 900 g of small. The big individuals will be entirely measured (PFRAC = 100; PECHAN = 100). The small ones will be sub -sampled with a ratio of 1/10 (PFRAC = 900; PECHAN = 90).
In the check check_raising the comparison between the number in TB and the raised number in TC has been not taking into account decimals, but comparing the integer numbers.
}
\usage{
check_raising(ResultDataTB,ResultDataTC,year,wd,suffix)
}
\arguments{
\item{ResultDataTB}{Catch data table (TB) .}
\item{ResultDataTC}{Biological data table (TC).}
\item{year}{reference year for the analysis}
\item{wd}{Working directory selected by the user.}
\item{suffix}{Suffix string of the Logfile.}
}
\details{
The check must be executed year by year.
The checks execution is stopped if any mismatching record was found.
}
\value{
The function returns TRUE if no error occurs, FALSE if some error record was found.
}
\references{
Anonymus. 2017. MEDITS-Handbook. Version n. 9. MEDITS Working Group. 106 pp. http://www.sibm.it/MEDITS 2011/principaledownload.htm
}
\author{
I. Bitetto, W. Zupa
}
\examples{
wd=tempdir()
suffix= "2020-03-05_time_h17m44s55"
year=2007
check_raising(RoME::TB,RoME::TC,year,wd,suffix)
}
\keyword{raising}
\keyword{sub-sampling}
