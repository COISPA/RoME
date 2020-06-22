\name{check_year}
\alias{check_year}
\title{
Consistency check of YEAR field
}
\description{
THe function check if the currect value for YEAR field is reported in each data table.
}
\usage{
check_year(TA, TB, TC, TE, TL, years, wd, Errors)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
 \item{TA}{Haul data table according to MEDITS protocol (TA)}
  \item{TB}{Catch data table according to MEDITS protocol (TB)}
  \item{TC}{Biological data table according to MEDITS protocol (TC)}
  \item{TE}{Individual biological data table according to MEDITS protocol (TE)}
  \item{TL}{Litter data table according to MEDITS protocol (TL)}
  \item{years}{list of the unique YEAR values in haul data (TA) table}
  \item{wd}{working directory path defined by the user}
  \item{Errors}{logfile name}
}
\details{
TA, TB and TC tables are mandatory while TE and TL could be used where available (otherwise use NA value).
}
\value{
The function returns FALSE when errors are detected in the YEAR field of the data tables.
}
\references{Anonymus. 2017. MEDITS-Handbook. Version n. 9. MEDITS Working Group. 106 pp.https://www.sibm.it/MEDITS 2011/principaledownload.htm}
\author{I. Bitetto, W. Zupa}
\examples{
    wd <- tempdir()
    years <- unique(RoME::TA$YEAR)
    Errors <- file.path(wd,"Logfiles","Logfile.dat")

    check_year(TA=RoME::TA, TB=RoME::TB,
    TC=RoME::TC, TE=NA, TL=NA, years=years,
    wd=wd, Errors=Errors)
}

\keyword{ error }
