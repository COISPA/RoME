\name{graphs_TA}
\alias{graphs_TA}
\title{
Function for qualitative checks of shooting depth, warp length and wing opening in Haul data table (TA).}
\description{
Check consistency between shooting depth and warp length and between warp length and wing opening}
\usage{
graphs_TA(ResultDataTA,wd)
}
\arguments{
\item{ResultDataTA}{Haul data table according to MEDITS protocol (TA).
}
\item{wd}{Working directory selected by the user.
}
}
\details{
The consistency between shooting depth and warp length and between warp length and wing opening is a qualitative control (scatter plots stored automatically in Graphs directory) allowing to detect the occurrence of outliers.
}
\value{
The function returns always TRUE, because the outcome of the function is a warning that does not lock the execution of the RoME checks.
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
graphs_TA(TA,wd)
}
\keyword{shooting depth, warp length, wing opening}
