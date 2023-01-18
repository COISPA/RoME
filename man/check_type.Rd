\name{check_type}
\alias{check_type}
\title{
Consistency check of TYPE_OF_FILE field
}
\description{
The function checks if the currect value for TYPE_OF_FILE field is reported in each data table. This is a global function that runs with the data frames not filtered by year.
}
\usage{
check_type(TA, TB, TC, TE, TL, years, wd, Errors)
}
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
\details{TA, TB and TC tables are mandatory while TE and TL could be used where available (otherwise use NA value).}
\value{The function returns FALSE when errors are detected in the TYPE_OF_FILE field of the data tables.}
\references{Anonymus. 2017. MEDITS-Handbook. Version n. 9. MEDITS Working Group. 106 pp.https://www.sibm.it/MEDITS 2011/principaledownload.htm}
\author{I. Bitetto, W. Zupa}
\examples{

    wd <- tempdir()
    TL = NA
    years <- unique(RoME::TA$YEAR)
    Errors <- file.path(wd,"Logfiles","Logfile.dat")
    check_type(TA=RoME::TA,TB=RoME::TB,TC=RoME::TC,
    TE=NA,TL=NA,years=years,wd=wd,Errors=Errors)

}

\keyword{ error }
