\name{check_unique_valid_haul}
\alias{check_unique_valid_haul}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function checking that among hauls with the same code, only one must be valid.}
\description{Check the presence of unique valid haul codes.}
\usage{
check_unique_valid_haul(ResultDataTA,year,wd,suffix)
}
\arguments{
\item{ResultDataTA}{Haul data table according to MEDITS protocol (TA).}
\item{year}{reference year for the analysis.}
\item{wd}{Working directory selected by the user.}
\item{suffix}{Suffix string of the Logfile.}
}
\details{
This function produce an error, stopping the check procedure to avoid cascade errors.
}
\value{
The function returns TRUE if no error occurs, while FALSE is returned when there is more than one valide hauls. In the logfile is reported the list of all the records in which the inconsistency is detected.}
\references{
Anonymus. 2017. MEDITS-Handbook. Version n. 9. MEDITS Working Group. 106 pp. http://www.sibm.it/MEDITS 2011/principaledownload.htm
}
\author{
I. Bitetto, W. Zupa
}
\examples{
TA = RoME::TA
year=2012
wd=tempdir()
suffix= "2020-03-05_time_h17m44s55"
check_unique_valid_haul(TA,year,wd,suffix)
}
\keyword{valid hauls}
