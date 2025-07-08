\name{check_spawning_period}
\alias{check_spawning_period}
\title{
Check consistency of maturity stages with respect to the spawning period
}
\description{
This function checks whether maturity stages reported in TC (or TE) are consistent with the reproductive period of each species. It detects three situations:
\itemize{
  \item Immature individuals caught during the spawning period but with lengths above the bibliographic threshold (maximum L50 plus 20\%).
  \item Mature individuals caught outside the spawning period.
  \item Mature individuals caught outside the spawning period and with lengths smaller than the smallest mature individual reported in the literature.
}
The check uses bibliographic information from the \code{Maturity_parameters} table, including L50 values and spawning months.
}
\usage{
check_spawning_period(ResultDataTA, ResultDataTC, year,
Maturity_parameters, DataTargetSpecies, wd, suffix)
}
\arguments{
  \item{ResultDataTA}{Haul data table (TA) according to MEDITS protocol.}
  \item{ResultDataTC}{Biological data table (TC) or alternatively the individual data table (TE), according to MEDITS protocol.}
  \item{year}{Reference year for the analysis.}
  \item{Maturity_parameters}{Table containing bibliographic information on L50, reproductive season, and smallest mature size for species and sexes.}
  \item{DataTargetSpecies}{Reference table for target species, including faunistic categories and other relevant data.}
  \item{wd}{Working directory selected by the user, where logfiles and CSV outputs will be saved.}
  \item{suffix}{Suffix string to append to output filenames, useful to distinguish between multiple runs.}
}
\details{
The function reads information from \code{Maturity_parameters} to identify the reproductive period (spawning season) of each species and sex. It then compares each record in TC or TE with these periods:

\itemize{
  \item Immature individuals (maturity codes 0, 1, 2A) are flagged if they are captured during the spawning period and have lengths exceeding the bibliographic threshold (maximum L50 + 20\%).
  \item Mature individuals are checked to verify whether they were caught outside the defined spawning period.
  \item Among mature individuals outside the spawning period, those with lengths smaller than the smallest mature specimen reported in literature are additionally flagged.
}

Warnings are written both to a .dat logfile and to a CSV file for easier downstream analysis. The CSV output includes details on GSA, year, haul number, species, sex, length class, spawning period months, month of capture, and type of warning.
}
\value{
The function always returns \code{TRUE}, as it is designed to produce warnings rather than halt execution of further routines in the \code{RoME} package.
}
\references{
Anonymous. 2017. MEDITS-Handbook. Version n. 9. MEDITS Working Group. 106 pp. \url{http://www.sibm.it/MEDITS2011/principaledownload.htm}
}
\author{
W. Zupa, I. Bitetto
}
\examples{
wd <- tempdir()
suffix <- "2020-03-05_time_h17m44s55"
year <- 2007
TA <- RoME::TA
TC <- RoME::TC
check_spawning_period(
  ResultDataTA = TA,
  ResultDataTC = TC,
  year = year,
  Maturity_parameters = RoME::Maturity_parameters,
  DataTargetSpecies = RoME::DataTargetSpecies,
  wd = wd,
  suffix = suffix
)
}
\keyword{quality control}
