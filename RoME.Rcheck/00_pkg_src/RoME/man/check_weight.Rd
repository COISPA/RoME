\name{check_weight}
\alias{check_weight}
\title{
Function that checks if number of individuals and total weight collected in the haul are consistent.}
\description{
The total weight and total number in the haul have to be consistent. The check is quantitative if there is information related to averageindividual weight. Alternatively the check is qualitative (through a plot).}
\usage{
  check_weight(ResultDataTB,DataTargetSpecies,wd,suffix)
}
\arguments{
\item{ResultDataTB}{Catch data table according to MEDITS protocol (TB).
}
\item{DataTargetSpecies}{Information related to individual weight of target and not target species.}
\item{wd}{Working directory selected by the user.
}
\item{suffix}{Suffix string of the Logfile.
}
}
\details{
The check of the consistency of total weight and total number in TB is performed after computing the mean weight for each species in each haul; a list of the weight intervals is provided in the package (DataTargetSpecies); this list was created in on the basis of the data collected in GSA 10 and 18 from 1994 to 2014. Of course, the user should update and complete as much as possible the table in order to allow RoME to detect errors on the basis of the updated weight intervals. This check is divided in a quantitative control (using the ranges mentioned above) and a qualitative control (using plots of the mean weight). The function gives a warning message. A maximum of 20 graphs will have been displayed in the R console and a maximum of 20 .tif files (6 plots per file) will have been stored in Graphs directory. The species with at least an occurrence of 10 hauls are plotted and saved.
}
\value{
The function returns always TRUE, because the outcome of the function is a warning that does not lock the execution of the RoME checks. The presence of inconsistencies in the data is reported in the logfile stored in the "Logfiles" subdirectory of the "wd" user-defined directory.
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
check_weight(TB,DataTargetSpecies,wd,suffix)
}
\keyword{weight}
