\name{check_area}
\alias{check_area}
\title{
 Check if TX files have the same area
}
\description{
 The function works with data of a single year of survey and checks if TX files have the same area code.}
\usage{
check_area(DataTA, DataTB, DataTC, DataTD=NA,DataTT=NA, DataTE=NA, DataTL=NA, wd, suffix)
}
\arguments{
  \item{DataTA}{Haul data table according to MEDITS protocol (TA)}
  \item{DataTB}{Catch data table according to MEDITS protocol (TB)}
  \item{DataTC}{Biological data table according to MEDITS protocol (TC)}
  \item{DataTD}{}
  \item{DataTT}{}
  \item{DataTE}{Individual biological data table according to MEDITS protocol (TE)}
  \item{DataTL}{Litter data table according to MEDITS protocol (TL)}
  \item{wd}{working directory path defined by the user}
  \item{suffix}{Suffix string of the Logfile}
}
\details{
TA, TB and TC tables are mandatory while TD, TT, TE and TL could be used where available.}
\value{
The function returns TRUE if no error occurs, while FALSE is returned  when there are differences in the AREA code among the TX tables.
}
\references{
Anonymus. 2017. MEDITS-Handbook. Version n. 9. MEDITS Working Group. 106 pp. http://www.sibm.it/MEDITS 2011/principaledownload.htm
}
\author{
I. Bitetto, W. Zupa
}
\examples{
    wd <- tempdir()
    suffix=paste(as.character(Sys.Date()),format(Sys.time(), "_time h%Hm%Ms%OS0"),sep="")
    DataTA = MEDITS::TA
    DataTB = MEDITS::TB
    DataTC = MEDITS::TC
    DataTD = NA
    DataTE = NA
    DataTT = NA
    DataTL = NA

    check_area(DataTA, DataTB,DataTC,DataTD=NA,DataTT=NA,DataTE=NA,DataTL=NA, wd, suffix)
}

\keyword{error}
