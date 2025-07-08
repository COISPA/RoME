\name{check_smallest_mature}
\alias{check_smallest_mature}
\title{Check for mature individuals below literature thresholds in TC data}
\description{
  This function verifies the consistency of maturity information in TC data
  by detecting mature individuals whose lengths are smaller than the
  smallest mature size reported in the literature for each species and sex.
  Detected inconsistencies are saved in a CSV file for traceability and
  further review, while a concise summary is written in the log file.
}
\usage{
check_smallest_mature(ResultData, year, MaturityParameters, TargetSpecies,
  wd, suffix)
}
\arguments{
  \item{ResultData}{
    Biological data table (TC) formatted according to the MEDITS protocol.
    This table should contain length-class, sex, and maturity stage
    information for all recorded hauls.
  }
  \item{year}{
    Reference year for the analysis. Only records from the specified year
    are processed.
  }
  \item{MaturityParameters}{
    Table with bibliographic parameters describing the smallest mature
    individual observed for each species and sex, and the related reference
    sources.
  }
  \item{TargetSpecies}{
    Table containing information on target species, including taxonomic
    codes and faunistic categories used to identify relevant maturity
    checks.
  }
  \item{wd}{
    Working directory where the function writes log files and CSV outputs.
  }
  \item{suffix}{
    String appended to filenames of log and CSV files to distinguish outputs
    from different runs.
  }
}
\details{
  The function processes the TC data for the selected year and examines each
  species listed in the \code{MaturityParameters} table. For each mature
  individual (i.e.\ not coded as immature stages), it checks whether the
  length class is smaller than the bibliographic minimum threshold,
  allowing a 10\% tolerance buffer to account for biological variability.
  If such individuals are detected, they are recorded in a CSV file
  including:
  \itemize{
    \item GSA
    \item Year
    \item Species
    \item Sex
    \item Haul number
    \item Length class (mm)
    \item Threshold applied (mm)
    \item Literature threshold (mm)
    \item Bibliographic reference
    \item Type of file
    \item Type of warning
  }

  A concise message summarizing how many records were written to the CSV is
  printed to the log file (\code{.dat}). If no inconsistencies are found,
  the function logs a message indicating that all maturity stages are above
  bibliographic thresholds.

  The check uses the updated \code{DataTargetSpecies} table for consistency
  with current species definitions and codes.
}
\value{
  The function always returns \code{TRUE}. Its purpose is to perform quality
  checks and log potential issues, but it does not interrupt the execution
  of other routines in the \code{RoME} package.
}
\references{  Anonymous. 2017. MEDITS-Handbook. Version n.\ 9. MEDITS Working Group. 106 pp. \url{http://www.sibm.it/MEDITS%202011/principaledownload.htm}}
\author{W. Zupa,  I. Bitetto}
\examples{
  wd <- tempdir()
  suffix <- "2020-03-05_time_h17m44s55"
  TC <- RoME::TC
  year <- 2007
  check_smallest_mature(
    TC,
    year,
    RoME::Maturity_parameters,
    RoME::DataTargetSpecies,
    wd,
    suffix
  )
}
\keyword{misc}
