\name{create_length}
\alias{create_length}
\title{
Function to create the R-sufi file taille.
}
\description{
This function report the information contained in the biological data table (TC) from the MEDITS protocol to the format required by R-sufi (Rochet et al., 2004).
}
\usage{
 create_length(Result,TM_list,wd)
}
\arguments{
\item{Result}{Biological data table according to MEDITS protocol (TC)}
\item{TM_list}{TM_list reference list}
\item{wd}{working directory path defined by the user}
}
\value{
The function saves automatically in the files R-Sufi folder the table taille in.csv format, with suffix of the year and GSA.}
\references{
Rochet M. J., V. M. Trenkel, J. A. Bertrand & J.-C. Poulard, 2004. R routines for survey based fisheries population and community indicators (R-SUFI). Ifremer, Nantes. Limited distribution.
Anonymus. 2017. MEDITS-Handbook. Version n. 9. MEDITS Working Group. 106 pp.https://www.sibm.it/MEDITS 2011/principaledownload.htm}
\author{I. Bitetto, W. Zupa}
\examples{
library(MEDITS)
library(RoME)
wd <- tempdir()
suffix="27-02-2020 18:30"
create_length(TC,TM_list,wd)
}

\keyword{R-sufi,taille}
