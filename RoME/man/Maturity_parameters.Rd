\name{Maturity_parameters}
\alias{Maturity_parameters}
\docType{data}
\title{
Maturity parameters }
\description{
Maturity parameters used for the checks: check_smallest_mature, check_spawning_period and check_sex_inversion
}
\usage{data("Maturity_parameters")}
\format{
  A data frame with 64 observations on the following 12 variables.
  \describe{
    \item{\code{Species}}{a factor with levels the rubincodes of the species for which the information is known.}
    \item{\code{SEX}}{a factor with levels \code{C} \code{F} \code{M}}
    \item{\code{min_L50}}{a numeric vector}
    \item{\code{max_L50}}{a numeric vector}
    \item{\code{smallest_mature_individual_observed}}{a numeric vector}
    \item{\code{min_length_SEX_INVERSION}}{a numeric vector}
    \item{\code{max_length_SEX_INVERSION}}{a numeric vector}
    \item{\code{Type_of_hermaphroditism}}{a factor with levels \code{} \code{proterandrous} \code{protogynous}}
    \item{\code{Area}}{a factor with levels as the area of the relevant information}
    \item{\code{Start_reproductive_season}}{a numeric vector}
    \item{\code{End_reproductive_season}}{a numeric vector}
  }
}
\source{
Literature and others}
\examples{
data(Maturity_parameters)
}
\keyword{datasets}
