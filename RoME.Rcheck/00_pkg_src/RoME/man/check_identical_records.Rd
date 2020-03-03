\name{check_identical_records}
\alias{check_identical_records}
\title{
Check of identical records in TX tables
}
\description{The function checks wherether there is one or more identical records in the selected type of table (TX).}
\usage{
check_identical_records(Data, wd, suffix)
}
\arguments{
 \item{Data}{one of the different data tables defined by the MEDITS protocol (TX)}
  \item{wd}{working directory path defined by the user}
  \item{suffix}{Suffix string of the Logfile}
}
\details{
The routine uses all the table format (TX) defined by the MEDITS protocol.}
\value{
The function returns TRUE if no error occurs, while FALSE is returned when there is one or more identical record in the given TX table.
}
\references{Anonymus. 2017. MEDITS-Handbook. Version n. 9. MEDITS Working Group. 106 pp.https://www.sibm.it/MEDITS 2011/principaledownload.htm}
\author{I. Bitetto, W. Zupa}
\examples{
    wd <- tempdir()
    suffix="27-02-2020 18:30"
    Data = MEDITS::TA
    check_identical_records(Data, wd, suffix)
}

\keyword{error}
