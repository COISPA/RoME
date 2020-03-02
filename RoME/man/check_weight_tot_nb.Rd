\name{check_weight_tot_nb}
\alias{check_weight_tot_nb}
\title{
Function that verifies the consistency between weight not null and number of individuals not null in catch data table (TB).
}
\description{
This check verifies,  if for the records with total weight not null, that there is a total number not null, except for categories V, G, H, D and E, as reported in MEDITS manual. }
\usage{
check_weight_tot_nb(ResultDataTB,wd,suffix)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{ResultDataTB}{Catch data table according to MEDITS protocol (TB).
}
\item{wd}{Working directory selected by the user.
}
\item{suffix}{Suffix string of the Logfile.
}
}
\details{
If total weight is different from 0, total number must be different from 0 (only if the category of the species is different from "E") and vice versa (for all faunistic categories.}
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
check_weight_tot_nb(TB,wd,suffix)
}
\keyword{weight,number}
