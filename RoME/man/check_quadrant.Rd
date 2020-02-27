\name{check_quadrant}
\alias{check_quadrant}
\title{
Check start and end quadrant for each haul
}
\description{
Function checking that the shooting quadrant and the hauling quadrant are the same.

}
\usage{
check_quadrant(ResultDataTA,wd,suffix)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ResultDataTA}{Haul data table according to MEDITS protocol (TA).
}
\item{wd}{Working directory selected by the user.
}
\item{suffix}{Suffix string of the Logfile.
}
}
\details{
The function returns a warning if shooting and hauling quadrant are not the same.
}
\value{
The function returns always TRUE, because the outcome of the function is a warning that doesnot lock the execution of the RoME checks.
}
\references{
For the definition of the quadrants, please refer to:
Anonymus. 2017. MEDITS-Handbook. Version n. 9. MEDITS Working Group. 106 pp. http://www.sibm.it/MEDITS%202011/principaledownload.htm
}
\author{
I. Bitetto, W. Zupa
}

\examples{
library(MEDITS)
library(RoME)
wd=tempdir()
suffix=paste(as.character(Sys.Date()),format(Sys.time(), "_time h%Hm%Ms%OS0"),sep="")
check_quadrant<-function(ResultDataTA,wd,suffix)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{quadrant}% use one of  RShowDoc("KEYWORDS")
