\name{create_length}
\alias{create_length}
\title{
Function to create the R-sufi file taille.
}
\description{
This function reports the information contained in the biological data table (TC) from the MEDITS protocol to the format required by R-sufi (Rochet et al., 2004).
}
\usage{
 create_length(ResultData,year,DataSpecies=RoME::TM_list,wd,save=TRUE)
}
\arguments{
\item{ResultData}{Biological data table according to MEDITS protocol (TC)}
\item{year}{reference year for the analysis}
\item{DataSpecies}{TM_list reference list}
\item{wd}{working directory path defined by the user}
\item{save}{boolean value to save the results in a csv file}
}
\details{
For the file taille the change in maturity scale in 2006 has been taken into account: from 1994 to 2005 the males of crustaceans have stage NA, because they were not staged until 2005. From 2006 they are considered mature for the stages strictly greater than 2A as well as for females of crustaceans. Before 2006 the females of crustaceans are considered mature for stages strictly greater than 1.
Bony fish and cephalopods are considered mature from stage 3 until 2005 and then they are considered mature from stage 2B.
For selachians, the immature are always stage 1 and 2.}
\value{
The function saves by default in the files R-Sufi folder the table taille in.csv format, with suffix of the year and GSA. If \code{save} parameter is FALSE the function returns the data frame as output.}
\references{
Rochet M. J., V. M. Trenkel, J. A. Bertrand & J.-C. Poulard, 2004. R routines for survey based fisheries population and community indicators (R-SUFI). Ifremer, Nantes. Limited distribution.
Anonymus. 2017. MEDITS-Handbook. Version n. 9. MEDITS Working Group. 106 pp.https://www.sibm.it/MEDITS 2011/principaledownload.htm}
\author{I. Bitetto, W. Zupa}
\examples{
  ResultData = RoME::TC
  year=2012
  DataSpecies=RoME::TM_list
  wd <- tempdir()
  create_length(ResultData,year,DataSpecies,wd,save=FALSE)
}

\keyword{R-sufi}
\keyword{taille}
