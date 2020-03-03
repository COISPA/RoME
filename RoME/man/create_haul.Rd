\name{create_haul}
\alias{create_haul}
\title{
Function to create R-sufi file containing haul data.
}
\description{
This function report the information contained in the hauld data table (TA) from the MEDITS protocol to the format required by R-sufi (Rochet et al., 2004).}
\usage{
create_haul(ResultDataTA,wd,suffix)
}

\arguments{
\item{DataTA}{Haul data table according to MEDITS protocol (TA)}
\item{wd}{working directory path defined by the user}
\item{suffix}{Suffix string of the Logfile}
}
\value{
The function saves automatically in the files R-Sufi folder the table "traits" in.csv format, with suffix of the year and GSA.}
\references{
Rochet M. J., V. M. Trenkel, J. A. Bertrand & J.-C. Poulard, 2004. R routines for survey based fisheries population and community indicators (R-SUFI). Ifremer, Nantes. Limited distribution.
Anonymus. 2017. MEDITS-Handbook. Version n. 9. MEDITS Working Group. 106 pp.https://www.sibm.it/MEDITS 2011/principaledownload.htm}
\author{I. Bitetto, W. Zupa}
\examples{
library(MEDITS)
library(RoME)
wd <- tempdir()
suffix="27-02-2020 18:30"
create_haul(TA,wd,suffix)
}

\keyword{R-sufi,traits}
