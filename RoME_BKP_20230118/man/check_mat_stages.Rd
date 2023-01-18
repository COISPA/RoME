\name{check_mat_stages}
\alias{check_mat_stages}
\title{
Consistency of maturity stages}
\description{Consistency check of maturity stages, according to the faunistic category (TM_list included in the package), sex and species}
\usage{
check_mat_stages(Data, wd, suffix, DataTargetSpecies = RoME::DataTargetSpecies,
DataSpecies = RoME::TM_list, stages = RoME::mat_stages)
}
\arguments{
  \item{Data}{Biological data table (TC) or individual biological data table (TE) according to MEDITS protocol }
    \item{wd}{working directory path defined by the user}
    \item{suffix}{Suffix string of the Logfile}
  \item{DataTargetSpecies}{data frame of species information as defined by the DataTargetSpecies dataset included in the package}
  \item{DataSpecies}{data frame with the list of the species as defined by the TM_list dataset included in the package}
  \item{stages}{data frame with the list of allowed maturity stages for faunistic category as defined by the mat_stages dataset included in the package}
}
\details{The check on maturity stage is performed for all the species using the new TM list, where selachians and bony fish are distinguished.}
\value{The function returns TRUE if no error occurs, while FALSE is returned when inconsistences in the maturity stages are detected. This function also gives a warning message, because it is difficult to define for all GSAs the year of passing from the "old" MEDITS maturity scale to the current MEDITS scale. Furthermore, the function returns an error (FALSE value) in case the faunistic category reported in the TC (or TE) table is present in the TM the one expected for the given species.}
\references{Anonymus. 2017. MEDITS-Handbook. Version n. 9. MEDITS Working Group. 106 pp. http://www.sibm.it/MEDITS 2011/principaledownload.htm}
\author{I. Bitetto, W. Zupa}
\examples{
\donttest{library(RoME)
DataTC <- RoME::TC
wd=tempdir()
suffix= "2020-03-05_time_h17m44s55"
check_mat_stages(DataTC, wd, suffix)
}
}
\keyword{ error }
\keyword{ warning }
