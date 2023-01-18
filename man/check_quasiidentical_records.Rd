\name{check_quasiidentical_records}
\alias{check_quasiidentical_records}
\title{
Function checking the presence of quasi-identical records.
}
\description{
Two or more "quasi-identical records" occurred when all the fields are respectively equal, except:
TYPE_OF_FILE, AREA, GEAR, VESSEL, YEAR, RIGGING, DOORS,  for TA table;
TYPE_OF_FILE, AREA, VESSEL, YEAR for TB and TC tables.These specifc fields are allowed to be identical.
}
\usage{
check_quasiidentical_records(Result,year,wd,suffix)
}
\arguments{
   \item{Result}{Haul data table according to MEDITS protocol (TA), or Catch data table (TB) or Biological data table (TC).}
   \item{year}{reference year for the analysis}
   \item{wd}{Working directory selected by the user.}
   \item{suffix}{Suffix string of the Logfile.}
}
\details{
The checks always returns TRUES generating warninng messages in the logfile if any quasi-identical record was found. In a given survey the following fields (of TA table) should be identical: 'TYPE_OF_FILE', 'AREA', 'VESSEL', 'GEAR', 'RIGGING', 'DOORS' and 'YEAR'. The function checks whether any differences occur in these fields in each yearly survey. The same think is done for all the other tables where these fields occur.
}
\value{
The function always returns TRUE reporting the presence of quasi-identical records in the logfile.
}
\references{
Anonymus. 2017. MEDITS-Handbook. Version n. 9. MEDITS Working Group. 106 pp. http://www.sibm.it/MEDITS 2011/principaledownload.htm
}
\author{
I. Bitetto, W. Zupa
}
\examples{
wd=tempdir()
suffix= "2020-03-05_time_h17m44s55"
year=2007
# example using TA table
TA <- RoME::TA
check_quasiidentical_records(TA,year,wd,suffix)

# example using TB table
TB <- RoME::TB
check_quasiidentical_records(TB,year,wd,suffix)

# example using TC table
TC <- RoME::TC
check_quasiidentical_records(TC,year,wd,suffix)

}
\keyword{quasi-identical}
