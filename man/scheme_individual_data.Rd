\name{scheme_individual_data}
\alias{scheme_individual_data}
\title{Summary of the individual data collected by species}
\description{Check for summarize the individual data collection (goodness of individual data sampling)}
\usage{
scheme_individual_data(DataTC, DataTE, year, wd, suffix)
}
\arguments{
  \item{DataTC}{Biological data table according to MEDITS protocol (TC)}
  \item{DataTE}{Individual biological data table according to MEDITS protocol (TE)}
  \item{year}{reference year for the analysis}
  \item{wd}{working directory path defined by the user}
  \item{suffix}{Suffix string of the Logfile}
}

\details{
The function uses biological data and individual biological data to produce a table where for each species are stored the number of length measurements, individual weights and number of otoliths taken by length class.
}
\value{
This check has as output a table (automatically saved in the wd user defined directory) named \code{sampling_individual_measures.csv} where for each species are stored the number of length measurements, individual weights and number of otoliths taken by length class. This table is useful to the user to evaluate the coverage of the individual measurements collections in order to verify if the sampling is in line with the protocol and to understand how eventually improve the sampling procedure.
}
\references{
Anonymus. 2017. MEDITS-Handbook. Version n. 9. MEDITS Working Group. 106 pp. http://www.sibm.it/MEDITS 2011/principaledownload.htm
}
\author{
I. Bitetto, W. Zupa
}
\examples{
\donttest{
    wd <- tempdir()
    suffix="2020-03-05_time_h17m44s55"
    DataTC = RoME::TC
    DataTE = RoME::TE
    year=2012
    scheme_individual_data(DataTC,DataTE,year, wd, suffix)
}
}

\keyword{ report }
