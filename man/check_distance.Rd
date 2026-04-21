\name{check_distance}
\alias{check_distance}
\title{Check of distance consistency}
\description{
The function checks whether there are inconsistencies between the \code{DISTANCE} field values in the TA table and the distances computed from the geographical coordinates of each haul.
}
\usage{
check_distance(DataTA, year, wd, suffix)
}
\arguments{
  \item{DataTA}{Haul data table formatted according to the MEDITS protocol (TA table).}
  \item{year}{Reference year for the analysis.}
  \item{wd}{Working directory path defined by the user.}
  \item{suffix}{Suffix string to uniquely identify the output logfile and plots.}
}
\details{
The comparison between the \code{DISTANCE} field and the distance computed from shooting and hauling coordinates is performed with a tolerance threshold of 30\%.

The geographic distance is calculated using the function \code{distGeo()} from the \code{geosphere} package, which computes the shortest path (great-circle distance) between two points on the Earth's ellipsoid (WGS84) using coordinates in decimal degrees.

Coordinates in the TA table are usually expressed in degrees and decimal minutes. These are automatically converted into decimal degrees using the internal function \code{MEDITS.to.dd()} before the computation.
}
\value{
The function always returns \code{TRUE}. Its purpose is to generate warnings for hauls where the recorded \code{DISTANCE} differs significantly from the computed geographic distance.

Warnings are saved in a logfile located in the \code{Logfiles} subdirectory of the specified working directory. Additionally, for hauls with discrepancies, graphical maps are generated and saved in the \code{Graphs} subdirectory to support visual inspection and correction.
}
\references{
Anonymous. 2017. MEDITS-Handbook. Version n. 9. MEDITS Working Group. 106 pp. \cr
\url{https://www.sibm.it/MEDITS\%202011/principaledownload.htm} \cr

Hijmans, R. J. (2019). \emph{geosphere: Spherical Trigonometry}. R package version 1.5-10. \cr
\url{https://cran.r-project.org/package=geosphere}
}
\author{W. Zupa, I. Bitetto}
\examples{
wd <- tempdir()
suffix <- "2020-03-05_time_h17m44s55"
year <- 2007
check_distance(RoME::TA, year, wd, suffix)
}
\keyword{quality-control}


