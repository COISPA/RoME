\name{check_date_haul}
\alias{check_date_haul}
\title{Check of date consistency}
\description{Check if in TB, TC and TE the date by haul is the same of the one reported in TA}
\usage{check_date_haul(DataTA, Data, wd, suffix)}
\arguments{
  \item{DataTA}{Haul data table according to MEDITS protocol (TA)}
  \item{Data}{Data frame of one of the following TX table: TB, TC, TE, TL}
  \item{wd}{working directory path defined by the user}
  \item{suffix}{Suffix string of the Logfile}
}
\details{The function check whethe in one of the TX file allowed in Data argument there are date consistent with the one reported in the haul data table (TA).}
\value{The function returns TRUE if no error occurs, while FALSE is returned when in the Date data frame there is one or more date not included in the TA tables.}
\references{
Anonymus. 2017. MEDITS-Handbook. Version n. 9. MEDITS Working Group. 106 pp. http://www.sibm.it/MEDITS 2011/principaledownload.htm
}
\author{
I. Bitetto, W. Zupa
}
\examples{
library(RoME)
    wd <- tempdir()
    suffix="27-02-2020 18:30"
    DataTA = MEDITS::TA
    Data = MEDITS::TB
    check_date_haul(DataTA, Data, wd, suffix)
}

\keyword{error}
