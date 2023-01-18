\name{create_catch}
\alias{create_catch}
\title{
Function to create the R-sufi file capt.}
\description{
This function report the information contained in the biological data table (TB) from the MEDITS protocol to the format required by R-sufi (Rochet et al., 2004).
}
\usage{
create_catch(ResultDataTB,year,wd,save=TRUE)
}
\arguments{
\item{ResultDataTB}{Catch data table according to MEDITS protocol (TB)}
\item{year}{reference year for the analysis}
\item{wd}{working directory path defined by the user}
\item{save}{boolean value to save the results in a csv file}
}
\value{
The function saves by default in the files R-Sufi folder the table capt in.csv format, with suffix of the year and GSA. If save parameter is FALSE the function returns the data frame as output.}
\references{
Rochet M. J., V. M. Trenkel, J. A. Bertrand & J.-C. Poulard, 2004. R routines for survey based fisheries population and community indicators (R-SUFI). Ifremer, Nantes. Limited distribution.
Anonymus. 2017. MEDITS-Handbook. Version n. 9. MEDITS Working Group. 106 pp.https://www.sibm.it/MEDITS 2011/principaledownload.htm}
\author{I. Bitetto, W. Zupa}
\examples{
TB = RoME::TB
year =2012
wd = tempdir()
create_catch(TB,year,wd,save=TRUE)
}

\keyword{R-sufi}
\keyword{Rcapt}
