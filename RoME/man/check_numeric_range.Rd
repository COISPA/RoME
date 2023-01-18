\name{check_numeric_range}
\alias{check_numeric_range}
\title{Check of the values range in specific fields}
\description{The function checks whether the values contained in specific fields are consistent within the allowed range of values.}
\usage{
check_numeric_range(Data, Field, Values, year, wd, suffix)
}
\arguments{
    \item{Data}{data table according to MEDITS protocol (TX)}
    \item{Field}{Name of the specific field of the selected TX table}
    \item{Values}{Vector of the allowed values for the field to be checked. The first two values are mandatory and indicate the extreme values of the range. The other optional values are single numerical exceptions to the field allowed values.}
    \item{year}{reference year for the analysis}
    \item{wd}{working directory path defined by the user}
    \item{suffix}{Suffix string of the Logfile}
}
\details{
The function checks the consistence of the values contained in specific fields with the relative allowed range of values. The function allorws to include exceptions to the allowed range of values for specific values. The check is performed on any of the "TX" data tables.
}
\value{The function returns TRUE if no error occurs, while FALSE is returned when inconsistencies are detected. The list of all the records in which the inconsistency is detected is reported in the logfile.}
\references{Anonymus. 2017. MEDITS-Handbook. Version n. 9. MEDITS Working Group. 106 pp.https://www.sibm.it/MEDITS 2011/principaledownload.htm}
\author{W. Zupa}
\examples{
    wd <- tempdir()
    suffix="2020-03-05_time_h17m44s55"
    Field = "SHOOTING_DEPTH"
    Values = c(10,800,0)
    Data = RoME::TA
    year <- unique(Data$YEAR)[1]
    check_numeric_range(Data, Field, Values, year, wd, suffix)
}

\keyword{ error }
