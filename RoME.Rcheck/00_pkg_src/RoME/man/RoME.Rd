\name{RoME}
\alias{RoME}
\title{
R code to perform multiple checks on MEDITS Survey data (TA, TB, TC, TE and TL files)}
\description{
The check is performed simultaneously on the files that can contain also data of more than one year.}
\usage{
RoME(TA, TB, TC, TE = NA, TL = NA, wd, suffix=NA)
}
\arguments{
  \item{TA}{Haul data table according to MEDITS protocol (TA)}
  \item{TB}{Catch data table according to MEDITS protocol (TB)}
  \item{TC}{Biological data table according to MEDITS protocol (TC)}
  \item{TE}{Individual biological data table according to MEDITS protocol (TE)}
  \item{TL}{Litter data table according to MEDITS protocol (TL)}
  \item{wd}{working directory path defined by the user}
  \item{suffix}{Suffix string of the Logfile}
}
\details{
...
}
\value{
The function does not correct the data, but it detects the errors, warning the user that there is the possibility of one or more errors, specifying the type of the error and easing the data correction.
}
\references{Anonymus. 2017. MEDITS-Handbook. Version n. 9. MEDITS Working Group. 106 pp.https://www.sibm.it/MEDITS 2011/principaledownload.htm}
\author{I. Bitetto, W. Zupa}
\examples{
    wd <- tempdir()
    suffix="2020-03-05_time h17m44s55"
    DataTA = RoME::TA
    DataTB = RoME::TB
    DataTC = RoME::TC
    DataTE = RoME::TE
    DataTL = RoME::TL
    RoME(DataTA, DataTB,DataTC,DataTE,DataTL, wd, suffix)
}

\keyword{error}
