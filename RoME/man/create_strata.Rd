\name{create_strata}
\alias{create_strata}
\title{
Function to create R-sufi file containing strata surface data.
}
\description{
This function reports the information contained in the stratification scheme for the selected area from the MEDITS protocol to the format required by R-sufi (Rochet et al., 2004).
}
\usage{
create_strata(Stratification,AREA,wd,save=TRUE)
}
\arguments{
\item{Stratification}{Stratification scheme according to MEDITS protocol.}
\item{AREA}{String of the GSA.}
\item{wd}{Working directory selected by the user.}
\item{save}{boolean value to save the results in a csv file}
}
\value{
The function saves automatically in the files R-Sufi folder the table strata in.csv format, with suffix of the year and GSA. If \code{save} parameter is FALSE the function returns the data frame as output.}
\references{
Rochet M. J., V. M. Trenkel, J. A. Bertrand & J.-C. Poulard, 2004. R routines for survey based fisheries population and community indicators (R-SUFI). Ifremer, Nantes. Limited distribution.
Anonymus. 2017. MEDITS-Handbook. Version n. 9. MEDITS Working Group. 106 pp.https://www.sibm.it/MEDITS 2011/principaledownload.htm}
\author{I. Bitetto, W. Zupa}
\examples{
  Stratification=RoME::stratification_scheme
  wd <- tempdir()
  AREA = 18
  create_strata(Stratification,AREA,wd,save=TRUE)
}

\keyword{R-sufi}
\keyword{strata}
