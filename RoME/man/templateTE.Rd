\name{templateTE}
\alias{templateTE}
\docType{data}
\title{
Template individual data table (TE).}
\description{
Dataframe containing the headers of TE, according to the MEDITS manual.}
\usage{data("templateTE")}
\format{
  A data frame with 0 observations on the following 43 variables.
  \describe{
    \item{\code{TYPE_OF_FILE}}{a logical vector}
    \item{\code{COUNTRY}}{a logical vector}
    \item{\code{AREA}}{a logical vector}
    \item{\code{VESSEL}}{a logical vector}
    \item{\code{YEAR}}{a logical vector}
    \item{\code{MONTH}}{a logical vector}
    \item{\code{DAY}}{a logical vector}
    \item{\code{HAUL_NUMBER}}{a logical vector}
    \item{\code{FAUNISTIC_CATEGORY}}{a logical vector}
    \item{\code{GENUS}}{a logical vector}
    \item{\code{SPECIES}}{a logical vector}
    \item{\code{LENGTH_CLASSES_CODE}}{a logical vector}
    \item{\code{SEX}}{a logical vector}
    \item{\code{NO_PER_SEX_MEASURED_IN_SUB_SAMPLE_FOR_OTOLITH}}{a logical vector}
    \item{\code{LENGTH_CLASS}}{a logical vector}
    \item{\code{MATURITY}}{a logical vector}
    \item{\code{MATSUB}}{a logical vector}
    \item{\code{INDIVIDUAL_WEIGHT}}{a logical vector}
    \item{\code{NO_PER_SEX_MEASURED_IN_SUB_SAMPLE_FOR_WEIGHT}}{a logical vector}
    \item{\code{OTOLITH_SAMPLED}}{a logical vector}
    \item{\code{NO_PER_SEX_MEASURED_IN_SUB_SAMPLE_FOR_AGEING}}{a logical vector}
    \item{\code{OTOLITH_READ}}{a logical vector}
    \item{\code{AGE}}{a logical vector}
    \item{\code{OTOLITH_CODE}}{a logical vector}
    \item{\code{RECORD_NUMBER}}{a logical vector}

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
\donttest{data(templateTE)}
}
\keyword{datasets}
