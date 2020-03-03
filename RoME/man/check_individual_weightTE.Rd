\name{check_individual_weightTE}
\alias{check_individual_weightTE}
\title{Consistency of individual weights (according to length-weight relationship)}
\description{
The finction checks the difference between observed and estimated individual weight in percentage.}
\usage{
check_individual_weightTE(DataTE, wd, suffix,verbose=FALSE)
}
\arguments{
  \item{DataTE}{Individual biological data table according to MEDITS protocol (TE)}
  \item{LW}{data frame of the length-weight parameters by species, area and sex}
  \item{wd}{working directory path defined by the user}
  \item{suffix}{Suffix string of the Logfile}
  \item{verbose}{bulean parameter, if TRUE returns messages about the progress of the elaboration}
}
\details{
For each individual is calculated the estimated weight according to length-weight relationship coefficient stored in LW table and the difference between observed and estimated individual weight in percentage.  Moreover, this function checks if for G1 species has been collected the weight or if has been entered the value ND, that is not allowed.
}
\value{
If for at least one record the difference between observed and estimated individual weight is greater than 20\% a warning is given in Logfile.dat and a table named TE_with_estimated_weights.csv is automatically produced in order to allow the user to easily eventually detect the errors. For all the records the percentage difference between observed and estimated weight is reported.
}
\references{Anonymus. 2017. MEDITS-Handbook. Version n. 9. MEDITS Working Group. 106 pp.https://www.sibm.it/MEDITS 2011/principaledownload.htm}
\author{I. Bitetto, W. Zupa}
\examples{
\donttest{
    wd <- tempdir()
    suffix="27-02-2020 18:30"
    DataTE = TE
    check_individual_weightTC(DataTE, wd=wd, suffix=suffix,vervose=TRUE)
    }
}
\keyword{ warning }
