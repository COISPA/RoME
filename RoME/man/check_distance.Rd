\name{check_distance}
\alias{check_distance}
\title{Check of distance consistency}
\description{
The function checks wherther there are inconsistencies between the DISTANCE field values and the computed distance.}
\usage{
check_distance(DataTA, wd, suffix)
}
\arguments{
    \item{DataTA}{Haul data table according to MEDITS protocol (TA)}
    \item{wd}{working directory path defined by the user}
    \item{suffix}{Suffix string of the Logfile}
}
\details{The comparison between distance in TA and computed distance is performed with a tolerance of 30\%. The formula used to compute the distance from the start and end coordinates is:\cr
\if{html}{\figure{distance01.png}{options: width="30\%"}}
\if{latex}{\figure{distance01.png}}\cr
where: \cr
\if{html}{\figure{distance02.png}{options: width="45\%"}}
\if{latex}{\figure{distance02.png}}\cr
and
\cr
\if{html}{\figure{distance03.png}{options: width="25\%"}}
\if{latex}{\figure{distance03.png}}\cr
All the coordinates involved in the formulas above are in decimal degrees; the routine converts automatically the coordinates listed in TA using the package MEDITS.
}
\value{The function returns always TRUE because the outcome of the function is a warning that does not lock the execution of the RoME checks. The presence of inconsistencies between DISTANCE field and computed distance is reported in the logfile stored in the "Logfiles"" subdirectory of the "wd" user-defined directory. For the hauls with a computed distance quite different from the distance recorded in TA a plot is produced and stored in Graphs directory to allow an easier correction.
}
\references{Anonymus. 2017. MEDITS-Handbook. Version n. 9. MEDITS Working Group. 106 pp.https://www.sibm.it/MEDITS 2011/principaledownload.htm}
\author{I. Bitetto, W. Zupa}
\examples{

library(MEDITS)
wd <- tempdir()
suffix="2020-03-05_time_h17m44s55"
library(RoME)
check_distance(MEDITS::TA,wd,suffix)
}
\keyword{ warning }
