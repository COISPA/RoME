\name{RSufi_files}
\alias{RSufi_files}
\title{
Function to concatenate the R-sufi files of the different years.
}
\description{
When the check procedure is completed for a number of years, it is possible to obtain the 4 R-Sufi global files from an year to another year.}
\usage{
RSufi_files(Year_start,Year_end,AREA,wd)
}
\arguments{
\item{Year_start}{Start year}
\item{Year_end}{Start end}
\item{AREA}{String of the GSA. Include only the number.}
\item{wd}{working directory path defined by the user}
}
\value{
The function saves automatically in the files R-Sufi folder the 4 global files, with suffix of the year range and GSA.}
\references{
Rochet M. J., V. M. Trenkel, J. A. Bertrand & J.-C. Poulard, 2004. R routines for survey based fisheries population and community indicators (R-SUFI). Ifremer, Nantes. Limited distribution.
Anonymus. 2017. MEDITS-Handbook. Version n. 9. MEDITS Working Group. 106 pp.https://www.sibm.it/MEDITS 2011/principaledownload.htm}
\author{I. Bitetto, W. Zupa}
\examples{
library(MEDITS)
library(RoME)
wd <- tempdir()

create_haul(TA,wd)
create_catch(TB,wd)
create_length(TC,TM_list,wd)
create_strata(stratification_scheme,"10",wd)
\donttest{RSufi_files(2007,2007,"10",wd)} # run only if you are working outside a temporary directory
}
\keyword{R-sufi}
\keyword{global files}
