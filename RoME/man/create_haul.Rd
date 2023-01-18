\name{create_haul}
\alias{create_haul}
\title{
Function to create R-sufi file containing haul data.
}
\description{
This function reports the information contained in the hauld data table (TA) from the MEDITS protocol to the format required by R-sufi (Rochet et al., 2004).}
\usage{
create_haul(ResultDataTA,year,wd,save=TRUE)
}

\arguments{
\item{ResultDataTA}{Haul data table according to MEDITS protocol (TA)}
\item{year}{reference year for the analysis}
\item{wd}{working directory path defined by the user}
\item{save}{boolean value to save the results in a csv file}
}
\value{
The function saves by default in the files R-Sufi folder the table traits in.csv format, with suffix of the year and GSA. If \code{save} parameter is FALSE the function returns the data frame as output.}
\references{
Rochet M. J., V. M. Trenkel, J. A. Bertrand & J.-C. Poulard, 2004. R routines for survey based fisheries population and community indicators (R-SUFI). Ifremer, Nantes. Limited distribution.
Anonymus. 2017. MEDITS-Handbook. Version n. 9. MEDITS Working Group. 106 pp.https://www.sibm.it/MEDITS 2011/principaledownload.htm}
\author{I. Bitetto, W. Zupa}
\examples{
TA = RoME::TA
year = 2012
wd = tempdir()
create_haul(TA,year,wd,save=FALSE)
}

\keyword{R-sufi}
\keyword{traits}
