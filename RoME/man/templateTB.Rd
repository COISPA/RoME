\name{templateTB}
\alias{templateTB}
\docType{data}
\title{
Template catch data table (TB).}
\description{
Dataframe containing the headers of TB, according to the MEDITS manual.}
\usage{data("templateTB")}
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
    \item{\code{CODEND_CLOSING}}{a logical vector}
    \item{\code{PART_OF_THE_CODEND}}{a logical vector}
    \item{\code{FAUNISTIC_CATEGORY}}{a logical vector}
    \item{\code{GENUS}}{a logical vector}
    \item{\code{SPECIES}}{a logical vector}
    \item{\code{NAME_OF_THE_REFERENCE_LIST}}{a logical vector}
    \item{\code{TOTAL_WEIGHT_IN_THE_HAUL}}{a logical vector}
    \item{\code{TOTAL_NUMBER_IN_THE_HAUL}}{a logical vector}
    \item{\code{NB_OF_FEMALES}}{a logical vector}
    \item{\code{NB_OF_MALES}}{a logical vector}
    \item{\code{NB_OF_UNDETERMINED}}{a logical vector}
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
\donttest{data(templateTB)}
}
\keyword{datasets}
