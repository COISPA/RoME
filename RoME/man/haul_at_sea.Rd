\name{haul_at_sea}
\alias{haul_at_sea}
\title{Check of haul position on sea area}
\description{
The function identify the hauls that don't fall in the user defined sea area.}
\usage{
haul_at_sea(DataTA, seas = RoME::MedSea, verbose = TRUE)
}

\arguments{
    \item{DataTA}{Haul data table according to MEDITS protocol (TA)}
  \item{seas}{polygon shapefile defining the extension of the sea area. The default \code{MedSea} dataset is referred to the Mediterranean and Black Sea area.}
  \item{verbose}{boolean varable returning verbose output if TRUE}
}
\details{
The function check whether the haul position falls in the polygon \code{seas} defining the extension of the reference sea area.
\cr
\if{html}{\figure{MedSeamap.png}{options: width="70\%"}}
\if{latex}{\figure{MedSeamap.png}}\cr

}
\value{
The function returns the list of the hauls out of the \code{seas} polygon. In case only starting haul positions are out of the sea's area a data frame is returned. If both starting and end positions are out of the polygon an object of class \code{list} is returned.
}
\references{Anonymus. 2017. MEDITS-Handbook. Version n. 9. MEDITS Working Group. 106 pp.https://www.sibm.it/MEDITS 2011/principaledownload.htm}
\author{I. Bitetto, W. Zupa}
\examples{
library(MEDITS)
wd <- tempdir()
suffix="2020-03-05_time h17m44s55"
haul_at_sea(MEDITS::TA, seas = MedSea, verbose = TRUE)
}
\keyword{list}
