\name{DataTargetSpecies}
\alias{DataTargetSpecies}
\docType{data}
\title{
Length and weight ranges}
\description{
Data related to the range for length and mean individual weight for all the relevant species. The year for which the species is target is also indicated.}
\usage{data("DataTargetSpecies")}
\format{
  A data frame with 392 observations on the following 10 variables.
  \describe{
    \item{\code{SPECIES}}{a factor with levels the rubincode of the species for which the information is present.}
    \item{\code{FAUNISTIC_CATEGORY}}{a factor with levels the old faunistic categories}
    \item{\code{MIN_WEIGHT}}{a numeric vector}
    \item{\code{MAX_WEIGHT}}{a numeric vector}
    \item{\code{UNIT_WEIGHT}}{grams}
    \item{\code{MIN_LEN}}{a numeric vector, in mm}
    \item{\code{MAX_LEN}}{a numeric vector, in mm}
    \item{\code{UNIT_LEN}}{mm}
    \item{\code{TARGET_START}}{a value indicating the year when the species entered in the target species list}
    \item{\code{TARGET_END}}{a value indicating the year when the species going out the target species list}
  }
}
\source{
Literaure and others}
\examples{
data(DataTargetSpecies)
}
\keyword{datasets}
