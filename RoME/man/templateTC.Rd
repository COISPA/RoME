\name{templateTC}
\alias{templateTC}
\docType{data}
\title{
Template biological data table (TC).}
\description{
Dataframe containing the headers of TC, according to the MEDITS manual.}
\usage{data("templateTC")}
\format{
  A data frame with 0 observations on the following 43 variables.
  \describe{
    \item{\code{TYPE_OF_FILE}}{a logical vector}
    \item{\code{COUNTRY}}{a logical vector}
    \item{\code{AREA}}{a logical vector}
    \item{\code{VESSEL}}{a logical vector}
    \item{\code{GEAR}}{a logical vector}
    \item{\code{RIGGING}}{a logical vector}
    \item{\code{DOORS}}{a logical vector}
    \item{\code{YEAR}}{a logical vector}
    \item{\code{MONTH}}{a logical vector}
    \item{\code{DAY}}{a logical vector}
    \item{\code{HAUL_NUMBER}}{a logical vector}
    \item{\code{CODEND_CLOSING}}{a logical vector}
    \item{\code{PART_OF_THE_CODEND}}{a logical vector}
    \item{\code{SHOOTING_TIME}}{a logical vector}
    \item{\code{SHOOTING_QUADRANT}}{a logical vector}
    \item{\code{SHOOTING_LATITUDE}}{a logical vector}
    \item{\code{SHOOTING_LONGITUDE}}{a logical vector}
    \item{\code{SHOOTING_DEPTH}}{a logical vector}
    \item{\code{HAULING_TIME}}{a logical vector}
    \item{\code{HAULING_QUADRANT}}{a logical vector}
    \item{\code{HAULING_LATITUDE}}{a logical vector}
    \item{\code{HAULING_LONGITUDE}}{a logical vector}
    \item{\code{HAULING_DEPTH}}{a logical vector}
    \item{\code{HAUL_DURATION}}{a logical vector}
    \item{\code{VALIDITY}}{a logical vector}
    \item{\code{COURSE}}{a logical vector}
    \item{\code{RECORDED_SPECIES}}{a logical vector}
    \item{\code{DISTANCE}}{a logical vector}
    \item{\code{VERTICAL_OPENING}}{a logical vector}
    \item{\code{WING_OPENING}}{a logical vector}
    \item{\code{GEOMETRICAL_PRECISION}}{a logical vector}
    \item{\code{BRIDLES_LENGTH}}{a logical vector}
    \item{\code{WARP_LENGTH}}{a logical vector}
    \item{\code{WARP_DIAMETER}}{a logical vector}
    \item{\code{HYDROLOGICAL_STATION}}{a logical vector}
    \item{\code{OBSERVATIONS}}{a logical vector}
    \item{\code{BOTTOM_TEMPERATURE_BEGINNING}}{a logical vector}
    \item{\code{BOTTOM_TEMPERATURE_END}}{a logical vector}
    \item{\code{MEASURING_SYSTEM}}{a logical vector}
    \item{\code{NUMBER_OF_THE_STRATUM}}{a logical vector}
    \item{\code{BOTTOM_SALINITY_BEGINNING}}{a logical vector}
    \item{\code{BOTTOM_SALINITY_END}}{a logical vector}
    \item{\code{MEASURING_SYSTEM_SALINITY}}{a logical vector}
  }
}
\details{
See Medits handbook.
}
\source{
The dataframe is empty and it is to be used to verify the correctness of headers.
}
\references{
Anonymus. 2017. MEDITS-Handbook. Version n. 9. MEDITS Working Group. 106 pp. http://www.sibm.it/MEDITS 2011/principaledownload.htm
}
\examples{
data(templateTC)
maybe str(templateTC)
}
\keyword{datasets}
