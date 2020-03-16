\name{assTL}
\alias{assTL}
\docType{data}
\title{
TL association between categories and sub-categories
}
\description{
List of the allowed association between categories and subcategories in litter data table (TL)
}
\usage{data("assTL")}
\format{
  A data frame with 42 observations on the following 2 variables.
  \describe{
    \item{\code{LITTER_CATEGORY}}{List of litter categories}
    \item{\samp{LITTER_SUB-CATEGORY}}{List of litter sub-categories}
  }
}
\details{
The table is used to check the correctness of the categories/sub-categories associations in litter data tables (TL).
}
\source{
MEDITS MEDITS-Handbook, Version n. 9 (2017)
}
\references{
Anonymus. 2017. MEDITS-Handbook. Version n. 9. MEDITS Working Group. 106 pp. http://www.sibm.it/MEDITS 2011/principaledownload.htm
}
\examples{
data(assTL)
## maybe str(assTL) ; plot(assTL) ...
}
\keyword{datasets}
