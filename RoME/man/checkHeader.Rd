\name{checkHeader}
\alias{checkHeader}
\title{
Function to check the correctness of the headers. }
\description{
Function to check the correctness of the headers for haul data (TA), catch data (TB), biological data (TC), individual data (TE), litter data (TL) tables. }
\usage{
checkHeader(dataframe, template,wd,suffix)}
\arguments{
  \item{dataframe}{ Table to check}
  \item{template}{ Template used for the check.}
  \item{wd}{working directory path defined by the user}
  \item{suffix}{Suffix string of the Logfile}
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
wd=tempdir()
suffix= "2020-03-05_time_h17m44s55"
checkHeader(RoME::TA,"TA",wd,suffix)
checkHeader(RoME::TB,"TB",wd,suffix)
checkHeader(RoME::TC,"TC",wd,suffix)

}
\keyword{headers}
