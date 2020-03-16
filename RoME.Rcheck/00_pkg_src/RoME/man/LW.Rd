\name{LW}
\alias{LW}
\docType{data}
\title{
Table of the Length-Weight parameters}
\description{
Table of the length-weight relationship coefficients reported by species, area and sex.
}
\usage{data("LW")}
\format{
  A data frame with 460 observations on the following 5 variables.
  \describe{
    \item{\code{AREA}}{vector of the reference geographic area}
    \item{\code{SPECIES}}{reference species for the a and b parameters}
    \item{\code{SEX}}{reference sex for the a and b parameters}
    \item{\code{a}}{a parameters of the length-weight relationship function}
    \item{\code{b}}{b parameters of the length-weight relationship function}
  }
}
\details{
Table of the length-weight relationship coefficients a and b.}
\examples{
data(LW)
## maybe str(LW) ; plot(LW) ...
}
\keyword{datasets}
