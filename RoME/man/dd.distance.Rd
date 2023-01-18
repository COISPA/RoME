\name{dd.distance}
\alias{dd.distance}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Estimation of haul distance
}
\description{
Function to estimate the hauls length using TA (table A, hauls data) with coordinates in the decimal degrees format (dd.ddd). The distances could be returned expressed in meters, kilometers and nautical miles.
}
\usage{
dd.distance(data, unit = "m", verbose=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
data frame of the hauls data (TA, table A)
}
  \item{unit}{
string value indicating the measure unit of the distance. Allowed values: "m" for meters, "km" for kilometers and "NM" for nautical miles.
}
  \item{verbose}{
give verbose output reporting in the output the selected measure unit of the distance.
}
}
\details{
The TA file should be populated with coordinates in dicimal degrees format.
}
\value{
The function returns the vector of the distances expressed in the selected measure unit.
}
\author{
Walter Zupa
}


\examples{
TA.dd <- MEDITS.to.dd(TA)
dd.distance(TA.dd, unit="km", verbose=FALSE)
}

\keyword{coordinates}
\keyword{TA}
