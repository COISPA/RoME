\name{check_smallest_mature}
\alias{check_smallest_mature}
\title{
Function to verify the consistency of maturity information respect to the smallest mature individual observed in literature.
}
\description{
In this check mature individuals with length smaller than size of the smallest mature individuals reported in literature are detected}
\usage{
check_smallest_mature(ResultData,year,MaturityParameters,TargetSpecies,wd,suffix)
}
\arguments{
\item{ResultData}{alternatively: Biological data table (TC) and Individual data table (TE).}
\item{year}{reference year for the analysis}
\item{MaturityParameters}{Information related to sex and maturity from literature or other sources.}
\item{TargetSpecies}{Information related to target species.}
\item{wd}{Working directory selected by the user.}
\item{suffix}{Suffix string of the Logfile.}
}
\details{
In these checks are involved the information stored in Maturity_parameters data frame.}
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
wd=tempdir()
suffix= "2020-03-05_time_h17m44s55"
TC=RoME::TC
year=2007
check_smallest_mature(TC,year,RoME::Maturity_parameters,RoME::DataTargetSpecies,wd,suffix)
}
\keyword{smallest mature}
