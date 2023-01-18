\name{check_nb_TE}
\alias{check_nb_TE}
\title{
Consistency of number of individuals sampled for weight and ageing in TE
}
\description{
The function checks the consistency of number of individuals sampled for weight and ageing in TE
}
\usage{
check_nb_TE(DataTE, year, wd, suffix)
}
\arguments{
  \item{DataTE}{Individual biological data table according to MEDITS protocol (TE)}
  \item{year}{reference year for the analysis}
  \item{wd}{working directory path defined by the user}
  \item{suffix}{Suffix string of the Logfile}
}

\details{
this function verify the consistency of the check-fields:
\itemize{
   \item {NO_PER_SEX_MEASURED_IN_SUB_SAMPLE_FOR_OTOLITH}
   \item {NO_PER_SEX_MEASURED_IN_SUB_SAMPLE_FOR_WEIGHT}
   \item {NO_PER_SEX_MEASURED_IN_SUBSAMPLE_FOR_AGEING}
 }
These fields are compared to the number of records present in TE by sex, length class and haul.
}
\value{
The function returns FALSE in case incosistances are detected in the individual biological data table (TE)
}
\references{Anonymus. 2017. MEDITS-Handbook. Version n. 9. MEDITS Working Group. 106 pp.https://www.sibm.it/MEDITS 2011/principaledownload.htm}
\author{I. Bitetto, W. Zupa}
\examples{

    wd <- tempdir()
    suffix="2020-03-05_time_h17m44s55"
    DataTE = RoME::TE
    year=2012
    check_nb_TE(DataTE,year, wd, suffix)

}
\keyword{ error }
