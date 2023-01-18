\name{check_mat_stages}
\alias{check_mat_stages}
\title{
Consistency of maturity stages}
\description{Consistency check of maturity stages, according to the faunistic category and sex}
\usage{
check_mat_stages(Data, year, wd, suffix, stages = RoME::mat_stages)
}
\arguments{
    \item{Data}{Biological data table (TC) or individual biological data table (TE) according to MEDITS protocol }
    \item{year}{reference year for the analysis}
    \item{wd}{working directory path defined by the user}
    \item{suffix}{Suffix string of the Logfile}
    \item{stages}{data frame with the list of allowed maturity stages for faunistic category as defined by the mat_stages dataset included in the package}
}
\details{The check on maturity stage is performed for the species included in the new TM list, where selachians and bony fish are distinguished. The check is applied to the following faunistic categories: Ao, Ae, B, C and Bst. }
\value{The function always returns TRUE generating a warning message when inconsistences in the maturity stages are detected, being difficult to define for all GSAs the year in which occurred the switch from the "old" MEDITS maturity scale to the current MEDITS scale.}
\references{Anonymus. 2017. MEDITS-Handbook. Version n. 9. MEDITS Working Group. 106 pp. http://www.sibm.it/MEDITS 2011/principaledownload.htm}
\author{I. Bitetto, W. Zupa}
\examples{
\donttest{
DataTC <- RoME::TC
wd=tempdir()
suffix= "2020-03-05_time_h17m44s55"
check_mat_stages(DataTC,year=2007, wd, suffix)
}
}
\keyword{ error }
\keyword{ warning }
