\name{check_hauls_TATL}
\alias{check_hauls_TATL}
\title{Check presence of TA hauls in TL}
\description{Check if the hauls in TA are present in TL}
\usage{check_hauls_TATL(DataTA, DataTL, wd, suffix)}
\arguments{
  \item{DataTA}{Haul data table according to MEDITS protocol (TA)}
  \item{DataTL}{Litter data table according to MEDITS protocol (TL)}
  \item{wd}{working directory path defined by the user}
  \item{suffix}{Suffix string of the Logfile}
}
\details{The function checks whether all the hauls peresent in hauls data table (TA) are included in the litter data table (TL). }
\value{
The function returns always TRUE because the outcome of the function is a warning that does not lock the execution of the RoME checks. The list of the hauls not present in the TL table is reported in the logfile stored in the "Logfiles"" subdirectory of the "wd" user-defined directory.
}
\references{Anonymus. 2017. MEDITS-Handbook. Version n. 9. MEDITS Working Group. 106 pp.https://www.sibm.it/MEDITS 2011/principaledownload.htm}
\author{I. Bitetto, W. Zupa}
\examples{
\donttest{
    library(RoME)
    wd <- tempdir()
    suffix="2020-03-05_time_h17m44s55"
    DataTA = RoME::TA
    DataTL = RoME::TL
    check_hauls_TATL(DataTA,DataTL,wd,suffix)
}
}

\keyword{ warning }
