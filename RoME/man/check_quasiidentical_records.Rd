\name{check_quasiidentical_records}
\alias{check_quasiidentical_records}
\title{
Function checking the presence of quasi-identical records.
}
\description{
Two or more "quasi-identical records" occurred when all the fields are respectively equal, except:
???	TYPE_OF_FILE, AREA, GEAR, VESSEL, YEAR, RIGGING, DOORS, allowed to be equals for TA table;
???	TYPE_OF_FILE, AREA, VESSEL, YEAR for TB and TC tables.
}
\usage{
check_quasiidentical_records(Result,wd,suffix)
}
\arguments{
   \item{ResultData}{Haul data table according to MEDITS protocol (TA), or Catch data table (TB) or Biological data table (TC).
}
\item{wd}{Working directory selected by the user.
}
\item{suffix}{Suffix string of the Logfile.
}
}
\details{
The checks execution is stopped if any quasi-identical record was found.
}
\value{
The function returns TRUE if no error occurs, FALSE is at least a quasi-identical record was found.
}
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
check_quasiidentical_records(ResultDataTA,wd,suffix)
check_quasiidentical_records(ResultDataTB,wd,suffix)
check_quasiidentical_records(ResultDataTC,wd,suffix)
}
\keyword{quasi-identical}
