\name{check_associations_category_TL}
\alias{check_associations_category_TL}
\title{Check corretness of TL categories}
\description{Check corretness of association between category and sub-category in TL consistent according to INSTRUCTION MANUAL VERSION 9}
\usage{check_associations_category_TL(DataTL, assTL, wd, suffix)}
\arguments{
  \item{DataTL}{Litter data table (TL) according to MEDITS protocol.}
  \item{assTL}{data frame with the association between TL (litter table) categories and sub-categories}
  \item{wd}{working directory path defined by the user}
  \item{suffix}{Suffix string of the Logfile}
}

\details{
The package uses a table of association between TL categories and sub-categories that is resident in the data folder of the package as assTL.rda file. }
\value{
The function returns always TRUE, because the outcome of the function is a warning that doesnot lock the execution of the RoME checks. The presence of inconsistencies in the data is reported in the logfile stored in the "Logfiles"" subdirectory of the "wd"" user-defined directory.
}
\references{
Anonymus. 2017. MEDITS-Handbook. Version n. 9. MEDITS Working Group. 106 pp. http://www.sibm.it/MEDITS 2011/principaledownload.htm
}
\author{
I. Bitetto, W. Zupa
}
\examples{
\donttest{
    library(RoME)
    wd <- tempdir()
    suffix="2020-03-05_time_h17m44s55"
    DataTL = RoME::TL
    check_associations_category_TL(DataTL, assTL, wd, suffix)
}
}
\keyword{error}
