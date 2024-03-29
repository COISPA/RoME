\name{mat_stages}
\alias{mat_stages}
\docType{data}
\title{Table of maturity stages}
\description{Table of maturity stages}
\usage{data("mat_stages")}
\format{
  A data frame with 132 observations on the following 4 variables.
  \describe{
    \item{\code{TYPE_OF_FILE}}{a character vector}
    \item{\code{FAUNISTIC_CATEGORY}}{a character vector}
    \item{\code{SEX}}{a character vector}
    \item{\code{MATURITY}}{a character vector}
    \item{\code{MATSUB}}{a character vector}
    \item{\code{code}}{a character vector}
  }
}
\details{Table of maturity stages per faunistic category.The maturity scales adopted up to 2006 is also provided.}
\source{Anonymus. 2017. MEDITS-Handbook. Version n. 9. MEDITS Working Group. 106 pp. http://www.sibm.it/MEDITS 2011/principaledownload.htm}
\references{Anonymus. 2017. MEDITS-Handbook. Version n. 9. MEDITS Working Group. 106 pp. http://www.sibm.it/MEDITS 2011/principaledownload.htm}
\examples{
data(mat_stages)
str(mat_stages)
}
\keyword{datasets}
