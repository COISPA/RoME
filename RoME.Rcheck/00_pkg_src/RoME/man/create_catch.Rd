\name{create_catch}
\alias{create_catch}
\title{
Function to create the R-sufi file capt.}
\description{
This function report the information contained in the biological data table (TB) from the MEDITS protocol to the format required by R-sufi (Rochet et al., 2004).
}
\usage{
create_catch(Result,wd)
}
\arguments{
\item{Result}{Catch data table according to MEDITS protocol (TB)}
\item{wd}{working directory path defined by the user}
}
\value{
The function saves automatically in the files R-Sufi folder the table capt in.csv format, with suffix of the year and GSA.}
\references{
Rochet M. J., V. M. Trenkel, J. A. Bertrand & J.-C. Poulard, 2004. R routines for survey based fisheries population and community indicators (R-SUFI). Ifremer, Nantes. Limited distribution.
Anonymus. 2017. MEDITS-Handbook. Version n. 9. MEDITS Working Group. 106 pp.https://www.sibm.it/MEDITS 2011/principaledownload.htm}
\author{I. Bitetto, W. Zupa}
\examples{
library(MEDITS)
library(RoME)
wd <- tempdir()
suffix="27-02-2020 18:30"
create_catch(TB,wd)
}

\keyword{R-sufi,capt}
