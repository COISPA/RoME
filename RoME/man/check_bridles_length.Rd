\name{check_bridles_length}
\alias{check_bridles_length}
\title{
check of bridles length correctness
}
\description{
The function performs consistency checks of the values in the "BRIDLES_LENGTH" field of the hauls data table (TA).
}
\usage{
check_bridles_length(DataTA, year, wd, suffix)
}
\arguments{
    \item{DataTA}{Haul data table according to MEDITS protocol (TA)}
    \item{wd}{working directory path defined by the user}
    \item{suffix}{Suffix string of the Logfile}
    \item{year}{reference year for the analysis}
}
\details{
The field BRIDLES_LENGTH can assume value 100 between 10-200 m of depth or 150 between 200-800 m. The function highlights also that MEDITS handbook recommends to increase the bridle length to 200 m in depths deeper than 500 m, reporting a warning in the logfile. Empty (NA) records in "BRIDLES_LENGTH" will be eliminated being the presence of empty fields already checked by \code{check_no_empty_fields}
}
\value{
The function returns always TRUE, because the outcome of the function is a warning that does not lock the execution of the 'RoME' checks. The presence of inconsistencies in the data is reported in the logfile stored in the "Logfiles"" subdirectory of the "wd"" user-defined directory.
}
\references{Anonymus. 2017. MEDITS-Handbook. Version n. 9. MEDITS Working Group. 106 pp.https://www.sibm.it/MEDITS 2011/principaledownload.htm}
\author{I. Bitetto, W. Zupa}
\examples{
wd <- tempdir()
suffix="2020-03-05_time_h17m44s55"
check_bridles_length(RoME::TA, year=2012, wd, suffix)
}
\keyword{error}
