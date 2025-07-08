\name{MedSea}
\alias{MedSea}
\docType{data}
\title{Shapefile of Mediterranean and Black Sea area}
\description{Polygon shapefile describing the GFCM's Geographical subareas (GSAs)}
\usage{data("MedSea")}
\format{The shapefile is derived from the GFCM's Geographical subareas (GSAs) shapefile}
\details{Polygon shapefile describing the GFCM's Geographical subareas (GSAs) compressed with the \code{xz} type of compression.
\cr
\if{html}{\figure{MedSeamap.png}{options: width="70\%"}}
\if{latex}{\figure{MedSeamap.png}}\cr
}
\source{http://www.fao.org/fileadmin/user_upload/faoweb/GFCM/Maps/GSAs_simplified.zip}
\references{http://www.fao.org/gfcm/data/maps/gsas/en/}
\examples{
\dontrun{
  library(sp)
  data(MedSea)
  plot(MedSea)
}
}
\keyword{datasets}
