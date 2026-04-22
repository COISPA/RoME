\name{check_swept_area}
\alias{check_swept_area}
\title{Check Swept Area Calculation and Plot}
\description{
Performs data validation and swept area calculation for MEDITS TA-format data. The function verifies numeric consistency in key columns, computes mean depth and swept area, logs any detected issues, and generates a plot of swept area versus mean depth for visual inspection.
}
\usage{
check_swept_area(ResultDataTA, year, wd, suffix)
}
\arguments{
  \item{ResultDataTA}{
    A data.frame containing MEDITS hauls data in TA-format. The function uses the following columns: \code{YEAR}, \code{VALIDITY}, \code{HAUL_NUMBER}, \code{WING_OPENING}, \code{DISTANCE}, \code{SHOOTING_DEPTH}, \code{HAULING_DEPTH}, and \code{TYPE_OF_FILE}.
  }

  \item{year}{
    Numeric scalar. The year to be checked in the dataset.
  }

  \item{wd}{
    Character string. Path to the working directory where log files and plots will be saved.
  }

  \item{suffix}{
    Character string. Optional suffix to append to the output filenames. If NULL, a timestamp-based suffix is automatically generated.
  }
}
\value{
A logical value:
\itemize{
  \item \code{TRUE} if no errors were detected in the data.
  \item \code{FALSE} if at least one error was found and logged.
}
}
\details{
This function performs multiple validation steps on TA-format data for MEDITS hauls. It verifies that certain key columns contain numeric values and checks for missing or empty entries in critical fields such as \code{WING_OPENING} and \code{DISTANCE}.

For each haul, it calculates the mean depth as the average of \code{SHOOTING_DEPTH} and \code{HAULING_DEPTH}, and estimates the swept area in square kilometers using the formula \eqn{(WING\_OPENING / 10) \times DISTANCE / 10^6}{(WING_OPENING / 10) * DISTANCE / 10^6}. Any inconsistencies or errors encountered during these checks are written to a logfile stored in the \code{Logfiles} folder.
If the data contains valid entries, the function generates and saves a scatter plot showing the swept area against mean depth. The plot includes a background shading to visually separate the depth range 0-200 meters from deeper strata, assisting in the interpretation of data distribution across depth zones.
All output files are organized within subfolders named \code{Logfiles} and \code{Graphs} under the specified working directory.
}
\examples{
\donttest{
# Define working directory
wd <- tempdir()

# Load data
ResultDataTA <- RoME::TA

# Filter for specific area if needed
ResultDataTA <- ResultDataTA[ResultDataTA$AREA == 18, ]

# Run the check for year 2005
check_swept_area(ResultDataTA, year = 2005, wd = wd, suffix = NULL)
}
}
\author{W. Zupa}


