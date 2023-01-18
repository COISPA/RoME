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
    \item{\code{YEAR}}{a logical vector}
    \item{\code{HAUL_NUMBER}}{a logical vector}
    \item{\code{CODEND_CLOSING}}{a logical vector}
    \item{\code{PART_OF_THE_CODEND}}{a logical vector}
    \item{\code{GENUS}}{a logical vector}
    \item{\code{SPECIES}}{a logical vector}
    \item{\code{LENGTH_CLASSES_CODE}}{a logical vector}
    \item{\code{WEIGHT_OF_THE_FRACTION}}{a logical vector}
    \item{\code{WEIGHT_OF_THE_SAMPLE_MEASURED}}{a logical vector}
    \item{\code{SEX}}{a logical vector}
    \item{\code{NO_OF_INDIVIDUAL_OF_THE_ABOVE_SEX_MEASURED}}{a logical vector}
    \item{\code{LENGTH_CLASS}}{a logical vector}
    \item{\code{MATURITY}}{a logical vector}
    \item{\code{MATSUB}}{a logical vector}
    \item{\code{NUMBER_OF_INDIVIDUALS_IN_THE_LENGTH_CLASS_AND_MATURITY_STAGE}}{a logical vector}
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
\donttest{data(templateTC)}
}
\keyword{datasets}
