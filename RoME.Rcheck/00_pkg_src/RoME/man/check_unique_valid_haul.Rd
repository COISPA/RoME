\name{check_unique_valid_haul}
\alias{check_unique_valid_haul}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function checking that smong hauls with the same code, only one must be valid.}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
check_unique_valid_haul(ResultDataTA,wd,suffix)
}
\arguments{
\item{ResultDataTA}{Haul data table according to MEDITS protocol (TA).
}
\item{wd}{Working directory selected by the user.
}
\item{suffix}{Suffix string of the Logfile.
}
}
\details{
This function produce an error, stopping the chec kprocedure to avoid cascade errors.
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
library(MEDITS)
library(RoME)
wd=tempdir()
suffix= "27-02-2020 18:30"
check_unique_valid_haul(TA,wd,suffix)
}
\keyword{valid hauls}
