\name{check_no_empty_fields}
\alias{check_no_empty_fields}
\title{Check empty fields in TA, TB, TC, TE and TL}
\description{All the fields, except to HYDROLOGICAL_STATION and OBSERVATIONS, must be not empty for valid hauls}
\usage{
check_no_empty_fields(Data, year, wd, suffix)
}
\arguments{
  \item{Data}{one of the different data tables defined by the MEDITS protocol (TX)}
  \item{year}{reference year for the analysis}
  \item{wd}{working directory path defined by the user}
  \item{suffix}{Suffix string of the Logfile}
}
\details{
The routine uses all the table format (TX) defined by the MEDITS protocol.
}
\value{
The function returns TRUE if no error occurs, while FALSE is returned when there is one or more empty record in the given TX table.
}
\references{Anonymus. 2017. MEDITS-Handbook. Version n. 9. MEDITS Working Group. 106 pp.https://www.sibm.it/MEDITS 2011/principaledownload.htm}
\author{I. Bitetto, W. Zupa}
\examples{
    wd <- tempdir()
    suffix="2020-03-05_time_h17m44s55"
    Data = RoME::TA
    year=2007
    check_no_empty_fields(Data, year, wd, suffix)
}
\keyword{error}
