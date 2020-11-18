\name{graphs_TA}
\alias{graphs_TA}
\title{
Function for qualitative checks of shooting depth, warp length and wing opening in Haul data table (TA).}
\description{
Qualitative control (by means of 2 graphs) of relation between shooting depth e warp opening and between warp length e wing opening
}
\usage{
graphs_TA(DataTA, wd, suffix)
}
\arguments{
    \item{DataTA}{Haul data table according to MEDITS protocol (TA)}
    \item{wd}{working directory path defined by the user}
    \item{suffix}{Suffix string of the Logfile}
}
\details{
The function generate 2 graphs for qualitative controls.}
\value{
Two graphs are stored in the Graphs folder in the wd user defined directory
}
\references{Anonymus. 2017. MEDITS-Handbook. Version n. 9. MEDITS Working Group. 106 pp.https://www.sibm.it/MEDITS 2011/principaledownload.htm}
\author{I. Bitetto, W. Zupa}
\examples{
library(MEDITS)
wd <- tempdir()
suffix="2020-03-05_time_h17m44s55"
graphs_TA(MEDITS::TA,wd,suffix)
}
\keyword{ graph }

