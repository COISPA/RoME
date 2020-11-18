\name{create_length}
\alias{create_length}
\title{
Function to create the R-sufi file taille.
}
\description{
This function reports the information contained in the biological data table (TC) from the MEDITS protocol to the format required by R-sufi (Rochet et al., 2004).
}
\usage{
 create_length(ResultData,DataSpecies=RoME::TM_list,wd)
}
\arguments{
\item{ResultData}{Biological data table according to MEDITS protocol (TC)}
\item{DataSpecies}{TM_list reference list}
\item{wd}{working directory path defined by the user}
}
\details{
For the file taille the change in maturity scale in 2006 has been taken into account: from 1994 to 2005 the males of crustaceans have stage NA, because they were not staged until 2005. From 2006 they are considered mature for the stages strictly greater than 2A as well as for females of crustaceans. Before 2006 the females of crustaceans are considered mature for stages strictly greater than 1.
Bony fish and cephalopods are considered mature from stage 3 until 2005 and then they are considered mature from stage 2B.
For selachians, the immature are always stage 1 and 2.}
\value{
The function saves automatically in the files R-Sufi folder the table taille in.csv format, with suffix of the year and GSA.}
\references{
Rochet M. J., V. M. Trenkel, J. A. Bertrand & J.-C. Poulard, 2004. R routines for survey based fisheries population and community indicators (R-SUFI). Ifremer, Nantes. Limited distribution.
Anonymus. 2017. MEDITS-Handbook. Version n. 9. MEDITS Working Group. 106 pp.https://www.sibm.it/MEDITS 2011/principaledownload.htm}
\author{I. Bitetto, W. Zupa}
\examples{
library(MEDITS)
library(RoME)
wd <- tempdir()
suffix="2020-03-05_time_h17m44s55"
create_length(TC,TM_list,wd)
}

\keyword{R-sufi}
\keyword{taille}
