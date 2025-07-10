\name{check_position_in_Med}
\alias{check_position_in_Med}
\title{Check of haul position in Mediterranean Sea}
\description{The function checks whether the position of the haul is in the Mediterranean Sea area or falls on the land.}
\usage{
check_position_in_Med(DataTA, year, wd, suffix)
}
\arguments{
    \item{DataTA}{Haul data table according to MEDITS protocol (TA)}
    \item{year}{reference year for the analysis}
    \item{wd}{working directory path defined by the user}
    \item{suffix}{Suffix string of the Logfile}
}
\details{The function uses the \code{haul_at_sea()} function to check whether the position of the haul is in the Mediterranean Sea area or falls on the land.}
\value{The function returns a boolean value. It is FALSE in case one or more haul positions fall out of the Mediterranean Sea area defined by the shapefile \emph{MedSea} included in the package.}
\references{Anonymus. 2017. MEDITS-Handbook. Version n. 9. MEDITS Working Group. 106 pp.https://www.sibm.it/MEDITS 2011/principaledownload.htm}
\author{I. Bitetto, W. Zupa}
\examples{
    wd <- tempdir()
    suffix="2020-03-05_time h17m44s55"
    DataTA = RoME::TA
    year=2007
    check_position_in_Med(DataTA, year, wd, suffix)
}
\keyword{ error }
