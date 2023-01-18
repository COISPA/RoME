\name{check_individual_weightTC}
\alias{check_individual_weightTC}
\title{Check of observed and estimated total weight in the haul}
\description{
The function compares the obserbed
}
\usage{
check_individual_weightTC(DataTC,LW=NA,year,wd,suffix,verbose=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{DataTC}{Biological data table according to MEDITS protocol (TC)}
  \item{LW}{data frame of the a and b parameters by species, area and sex}
  \item{year}{reference year for the analysis}
  \item{wd}{working directory path defined by the user}
  \item{suffix}{Suffix string of the Logfile}
  \item{verbose}{bulean parameter, if TRUE returns messages about the progress of the elaboration}
}
\details{
The warning is given when difference between the sum of estimated individual weights (by haul, species and sub-samples) and the WEIGHT_OF_THE_SAMPLE_MEASURED is greater than 50\% for at least one record. This check is based on the table LW contained in package, where the length-weight relationship coefficients are reported by species, area and sex.}
\value{
The file Comparison_estimated_observed_weight_in_TC.csv is automatically saved in the working directory in order to easily detect the samples with this differences in total weight. For all the records the percentage difference between observed and estimated weight is reported.
}
\references{Anonymus. 2017. MEDITS-Handbook. Version n. 9. MEDITS Working Group. 106 pp.https://www.sibm.it/MEDITS 2011/principaledownload.htm}
\author{I. Bitetto, W. Zupa}
\examples{
    wd <- tempdir()
    suffix="2020-03-05_time_h17m44s55"
    TC = RoME::TC[1:20,]
    check_individual_weightTC(DataTC=TC,year=2007, wd=wd, suffix=suffix)
}

\keyword{warning}
