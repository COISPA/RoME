\name{check_step_length_distr}
\alias{check_step_length_distr}
\title{
The function verifies that in TC the length measures are reported with the correct precision.}
\description{
Fishes and cephalopods length classes must have full or half step (in case of LENGTH_CLASSES_CODE=1 only full). All the measures , must be integer numbers.}
\usage{
check_step_length_distr(ResultData,wd,suffix)
}
\arguments{
\item{ResultData}{alternatively: Biological data table (TC) and Individual data table (TE).}
\item{wd}{Working directory selected by the user.}
\item{suffix}{Suffix string of the Logfile.}
}
\details{
According to the MEDITS proptocol, Fishes and cephalopods length measurement must collected full or half step and all the measures , must be integer numbers.}
\value{
The function returns TRUE if no error occurs, while FALSE is returned when the step is not correctly used. In the logfile is reported the list of all the records in which the inconsistency is detected.}
\references{
Anonymus. 2017. MEDITS-Handbook. Version n. 9. MEDITS Working Group. 106 pp. http://www.sibm.it/MEDITS 2011/principaledownload.htm
}
\author{
I. Bitetto, W. Zupa
}
\examples{
library(MEDITS)
library(RoME)
wd=tempdir()
suffix= "27-02-2020 18:30"
check_step_length_distr(TC,wd,suffix)
}
\keyword{length}
\keyword{step}
