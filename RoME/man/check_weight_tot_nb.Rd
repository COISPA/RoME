\name{check_weight_tot_nb}
\alias{check_weight_tot_nb}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function to check if, when the weight is not null, also the number is not null.
}
\description{
If total weight is different from 0, total number must be different from 0 (only if the category of the species is different from "E") and vice versa (for all faunistic categories).
}
\usage{
check_weight_tot_nb(ResultDataTB,wd,suffix)
}
\arguments{
  \item{ResultDataTB}{Catch data table according to MEDITS protocol (TB)}
  \item{wd}{working directory path defined by the user}
  \item{suffix}{Suffix string of the Logfile}
}
\details{
In this check 'RoME' verifies if for the records with total weight not null, there is a total number not null, except for categories V, G, H, D and E, as reported in MEDITS manual.
}
\value{
The function returns always TRUE, because the outcome of the function is a warning that does not lock the execution of the 'RoME' checks.
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
suffix= "2020-03-05_time_h17m44s55"
check_weight_tot_nb(TB,wd,suffix)
}
\keyword{numberl}
\keyword{weight}
\keyword{not null}
