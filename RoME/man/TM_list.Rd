\name{TM_list}
\alias{TM_list}
\docType{data}
\title{
TM list
}
\description{
The present list is destined to code the marine species encountered in the Mediterranean. It has been built following the principle used in the Nordic Code Centre (Stockholm). For most of the species the codes are identical to those proposed by the NCC. However some species can be coded differently. In addition numerous Mediterranean species are not included in the NCC code and have been added. So the present list is specific. It has to be referred as the TM list (Taxonomic list not only Faunistic, FM list).
}
\usage{data("TM_list")}
\format{
  A data frame with 1470 observations on the following 11 variables.
  \describe{
    \item{\code{N.}}{a numeric vector}
    \item{\code{MeditsCode}}{a factor with levels of species codes in the RUBIN format (see MEDITS manual)}
    \item{\code{Scientific.Name.................................................valid}}{a factor with levels of the scientific names of the species}
    \item{\code{Authorship}}{a factor with levels of the authorship of the inforformation}
    \item{\code{Source}}{a factor with levels sources of the inforformation}
    \item{\code{Reference}}{a factor with levels of the bibliografic references}
    \item{\code{Remarks}}{a factor with levels the reported remarks}
    \item{\code{CATFAU}}{a factor with levels of the faunistic categories of the species}
    \item{\code{CODLON}}{a factor with CODLON that represents the Length classes code: m = 1 mm; 0 = 0,5 cm; 1 = 1 cm.}
    \item{\code{GSAs}}{a factor with levels of the Geographic Sub-Areas (GSA) adopted in the MEDITS protocol.}
    \item{\code{Year}}{a factor with levels of the years}
  }
}

\source{
MEDITS MEDITS-Handbook, Version n. 9 (2017)
}
\references{
Anonymus. 2017. MEDITS-Handbook. Version n. 9. MEDITS Working Group. 106 pp. http://www.sibm.it/MEDITS 2011/principaledownload.htm
}
\examples{
data(TM_list)
## maybe str(TM_list) ; plot(TM_list) ...
}
\keyword{datasets}
