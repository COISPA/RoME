\name{checkHeader}
\alias{checkHeader}
\title{
Function to check the correctness of the headers. }
\description{
Function to check the correctness of the headers for haul data (TA), catch data (TB), biological data (TC), individual data (TE), litter data (TL) tables. }
\usage{
checkHeader(dataframe, template)}
\arguments{
  \item{dataframe}{ Table to check}
  \item{template}{ Template used for the check.}
}
\details{
This function produce an error, stopping the check procedure to avoid cascade errors.
}
\value{
The function returns TRUE if no error occurs, while FALSE is returned when there is more than one valide hauls. In the logfile and in the console is reported the list of all the records in which the inconsistency is detected.}
\references{
Anonymus. 2017. MEDITS-Handbook. Version n. 9. MEDITS Working Group. 106 pp. http://www.sibm.it/MEDITS 2011/principaledownload.htm
}
\author{
I. Bitetto, W. Zupa
}
\examples{
library(MEDITS)
library(RoME)
wd=tempdir()
suffix= "27-02-2020 18:30"
checkHeader(TA,"TA")
checkHeader(TB,"TB")
checkHeader(TC,"TC")

}
\keyword{headers}
