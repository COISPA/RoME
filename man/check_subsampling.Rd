\name{check_subsampling}
\alias{check_subsampling}
\title{
Function to warn the user about the presence of subsamples <0.1 of the total catch.
}
\description{
Check if the sub-sample is smaller than the 10 percent of the total weight in the haul.
}
\usage{
 check_subsampling(ResultTC,year,wd,suffix)
}
\arguments{
\item{ResultTC}{Biological data table (TC).}
\item{year}{reference year for the analysis.}
\item{wd}{Working directory selected by the user.}
\item{suffix}{Suffix string of the Logfile.}
}
\details{
This function give a warning, repoted in the Logfile, if the sub-sample is unusually small respect to the total catch of the species.
}
\value{
The function returns always TRUE, because the outcome of the function is a warning that does not lock the execution of the 'RoME' checks. The presence of inconsistencies in the data is reported in the logfile stored in the "Logfiles" subdirectory of the "wd" user-defined directory.
}
\references{
For the definition of the strata, please refer to:
Anonymus. 2017. MEDITS-Handbook. Version n. 9. MEDITS Working Group. 106 pp. http://www.sibm.it/MEDITS 2011/principaledownload.htm
}
\author{
I. Bitetto, W. Zupa
}
\examples{
TC = RoME::TC
year=2007
wd=tempdir()
suffix= "2020-03-05_time_h17m44s55"
check_subsampling(TC,year,wd,suffix)
}
\keyword{sub-sampling}
