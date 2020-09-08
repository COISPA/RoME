\name{check_dictionary}
\alias{check_dictionary}
\title{Check of the dictionary of specific fields}
\description{The function checks whether the values contained in specific fields are consistent with the allowed values of the dictionaries.}
\usage{check_dictionary(ResultData, Field, Values, wd, suffix)}
\arguments{
    \item{ResultData}{Haul data table according to MEDITS protocol (TA)}
    \item{Field}{Name of the specific field of the selected TX table}
    \item{Values}{Vector of the allowed values for the field to be checked}
    \item{wd}{working directory path defined by the user}
    \item{suffix}{Suffix string of the Logfile}
}
\details{The function checks the consistence of the contained in specific fields with the relative allowed values. The check is performed on the hauls data table (TA), the catch data table (TB), the biological data table (TC) and the individual biological data (TE).}
\value{The function returns TRUE if no error occurs, while FALSE is returned when there are differences between the field values and the reference dictionaries. In the logfile is reported the list of all the records in which the inconsistency is detected.}
\references{Anonymus. 2017. MEDITS-Handbook. Version n. 9. MEDITS Working Group. 106 pp.https://www.sibm.it/MEDITS 2011/principaledownload.htm}
\author{I. Bitetto, W. Zupa}
\examples{
    wd <- tempdir()
    suffix="27-02-2020 18:30"
    Field = "COURSE"
    Values = c("R","N")
    DataTA = MEDITS::TA
    library(RoME)
    check_dictionary(ResultData = DataTA, Field, Values, wd, suffix)
}

\keyword{ error }
