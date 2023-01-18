\name{check_step_length_distr}
\alias{check_step_length_distr}
\title{
The function verifies that in TC the length measures are reported with the correct precision.}
\description{
Fishes and cephalopods length classes must have full or half step (in case of LENGTH_CLASSES_CODE=1 only full). All the measures , must be integer numbers.}
\usage{
check_step_length_distr(ResultData,year,wd,suffix)
}
\arguments{
\item{ResultData}{alternatively: Biological data table (TC) and Individual data table (TE).}
\item{year}{reference year for the analysis}
\item{wd}{Working directory selected by the user.}
\item{suffix}{Suffix string of the Logfile.}
}
\details{
According to the MEDITS protocol, Fishes and cephalopods length measurement must collected full or half step and all the measures, must be integer numbers. Empty (NA) records in LENGHT_CLASS field are removed from the analysis being empty fields already detected by \code{check_no_empty_fields} function.}
\value{
The function returns TRUE if no error occurs, while FALSE is returned when the step is not correctly used. In the logfile is reported the list of all the records in which the inconsistency is detected.}
\references{
Anonymus. 2017. MEDITS-Handbook. Version n. 9. MEDITS Working Group. 106 pp. http://www.sibm.it/MEDITS 2011/principaledownload.htm
}
\author{
I. Bitetto, W. Zupa
}
\examples{
data <- RoME::TC
wd=tempdir()
year = 2007
suffix= "2020-03-05_time_h17m44s55"
check_step_length_distr(data,year,wd,suffix)
}
\keyword{length}
\keyword{step}
