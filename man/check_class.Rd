\name{check_class}
\alias{check_class}
\title{
Check of field's class
}
\description{The function checks the class of the fields included in the selected table TX using the dictionary table reported in the class data frame.}
\usage{
check_class(data, tab, suffix, wd)
}
\arguments{
  \item{data}{one of the different data tables defined by the MEDITS protocol (TX)}
  \item{tab}{character string defining the type of table used in the analysis. Allowed values: "TA", "TB", "TC", "TE" and "TL".}
  \item{suffix}{Suffix string of the Logfile}
  \item{wd}{working directory path defined by the user}
}
\value{
The function returns TRUE if no error are detected, while FALSE value is returned if any of the checked fields in the selected table has a not expected class of data.
}
\references{Anonymus. 2017. MEDITS-Handbook. Version n. 9. MEDITS Working Group. 106 pp.https://www.sibm.it/MEDITS 2011/principaledownload.htm}
\author{W. Zupa}
\examples{
wd = tempdir()
check_class(data=RoME::TA, "TA", wd=wd,suffix="test_file")
check_class(data=RoME::TB, "TB", wd=wd,suffix="test_file")
check_class(data=RoME::TC, "TC", wd=wd,suffix="test_file")
check_class(data=RoME::TE, "TE", wd=wd,suffix="test_file")
check_class(data=RoME::TL, "TL", wd=wd,suffix="test_file")
}
