\name{check_spawning_period}
\alias{check_spawning_period}
\title{
Function to check the consistency of the maturity stages according to the spawning period.}
\description{
The mature individuals caught outside the spawning are detected; moreover, the mature individuals caught outside the spawning with size smaller than the size of smallest mature individual in bibliography and the immature individuals caught during the reproductive period, but with size greater than maximum L50 + 0.2 * L50 are detected.}
\usage{
check_spawning_period(ResultDataTA,ResultDataTC,
Maturity_parameters,DataTargetSpecies,wd,suffix)
}
\arguments{
  \item{ResultDataTA}{Haul data table(TA).}
  \item{ResultDataTC}{alternatively: Biological data table (TC) and Individual data table (TE).}
  \item{Maturity_parameters}{Information related to sex and maturity from literature or other sources.}
  \item{DataTargetSpecies}{Information related to reference species.}
\item{wd}{Working directory selected by the user.}
\item{suffix}{Suffix string of the Logfile.}
}
\details{
In this checks are involved the information stored in Maturity_parameters data.frame about the L50 and spawning period is used.)}
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
check_spawning_period(TA,TC,Maturity_parameters,DataTargetSpecies,wd,suffix)
}
\keyword{smallest mature}
