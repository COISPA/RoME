\name{check_smallest_mature}
\alias{check_smallest_mature}
\title{
Function to verify the consistency of maturity information respect to the smallest mature individual observed in literature.
}
\description{
In this check mature individuals with length smaller than size of the smallest mature individuals reported in literature are detected}
\usage{
check_smallest_mature(ResultData,Maturity_parameters,DataTargetSpecieswd,suffix)
}
\arguments{
\item{ResultData}{alternatively: Biological data table (TC) and Individual data table (TE).}
\item{Maturity_parameters}{Information related to sex and maturity from literature or other sources.}
\item{DataTargetSpecies}{Information related to target species.}
\item{wd}{Working directory selected by the user.}
\item{suffix}{Suffix string of the Logfile.}
}
\details{
In these checks are involved the information stored in Maturity_parameters data frame.}
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
suffix= "27-02-2020 18:30"
check_smallest_mature(TC,Maturity_parameters,DataTargetSpecieswd,suffix)
}
\keyword{smallest mature}
