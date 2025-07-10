\name{check_length}
\alias{check_length}
\title{Check of length classes in TC}
\description{
  The function checks the consistency of length classes in the TC table
  for the MEDITS survey. It verifies that the recorded length classes for
  each species fall within the expected ranges reported in a reference
  table of species-specific length limits. Warnings for inconsistencies
  are written to a CSV file for easier downstream analysis instead of
  being printed extensively in the log file.
}
\usage{
check_length(DataTC, DataSpecies = NA, year, wd, suffix,
             DataTargetSpecies = RoME::DataTargetSpecies)
}
\arguments{
  \item{DataTC}{Biological data table (TC) formatted according to the
    MEDITS protocol.}
  \item{DataSpecies}{A data frame with species-specific reference length
    limits. If not provided, the internal \code{DataTargetSpecies}
    dataset included in the package is used by default.}
  \item{year}{Reference year for the analysis.}
  \item{wd}{Working directory path defined by the user, where log files
    and CSV output will be saved.}
  \item{suffix}{Suffix string for naming the log and CSV files, allowing
    unique identification of output files from different runs.}
  \item{DataTargetSpecies}{Reference dataset with species-specific
    information on length ranges, used as default if
    \code{DataSpecies} is not provided.}
}
\details{
  The function filters the TC data for the selected year and checks each
  length class recorded against the minimum (\code{LMIN01}) and maximum
  (\code{LMAX99}) length values defined for the corresponding species in
  the reference table. It also checks for negative or missing length
  class values. All detected issues are collected and saved in a CSV file
  for clear reporting, instead of generating verbose outputs in the log
  file.

  Species with less than 50 observations in TC are excluded from the check
  by default, to focus the analysis on more frequently recorded species.
  The function ensures robust handling of missing (\code{NA}) values in
  both the TC table and the reference datasets to avoid false warnings.

  A short summary of the check results is written to the \code{.dat}
  logfile. If no inconsistencies are detected, a message indicating
  successful validation is saved.
}
\value{
  Returns \code{TRUE}, invisibly. Detected inconsistencies are written to
  a CSV file in the working directory; if no issues are found, the CSV
  file remains empty apart from the header row.
}
\references{Anonymus. 2017. MEDITS-Handbook. Version n. 9. MEDITS Working Group. 106 pp.https://www.sibm.it/MEDITS 2011/principaledownload.htm}
\author{W. Zupa, I. Bitetto}
\examples{
  wd <- tempdir()
  DataTC <- RoME::TC[1:20,]
  DataSpecies <- NA
  suffix <- "2020-03-05_time_h17m44s55"
  check_length(DataTC, DataSpecies = NA, year = 2007, wd, suffix)
}
