\name{check_weight}
\alias{check_weight}
\title{
Check consistency between total weight and number of individuals in MEDITS TB data.
}
\description{
This function performs quality checks on the consistency between the total weight and the total number of individuals recorded in the MEDITS TB table. The consistency check is carried out both quantitatively-using known reference ranges of mean weights for each species-and qualitatively-via plots displaying the observed mean weights for each species across hauls.
}
\usage{
  check_weight(ResultDataTB, year, DataTargetSpecies, wd, suffix)
}
\arguments{
\item{ResultDataTB}{
Catch data table formatted according to the MEDITS protocol (table TB). This data includes information on species, number of individuals, and total weight per haul.
}
\item{year}{
Reference year for the analysis. Only hauls from the specified year will be analysed.
}
\item{DataTargetSpecies}{
Table providing reference weight ranges for species. It contains the lower (5th percentile) and upper (95th percentile) bounds for the mean weight of individuals per species, based on historical data collected from 2012 to 2022 across all surveyed GSAs. This table should be kept updated to reflect the latest knowledge about species biology in the Mediterranean Sea.
}
\item{wd}{
Working directory chosen by the user. This directory will contain subfolders where logs and plots will be saved.
}
\item{suffix}{
Suffix string appended to the filenames of the log files. Useful for distinguishing outputs from multiple runs.
}
}
\details{
The function operates in two phases:

\strong{1. Quantitative Check:}

\itemize{
  \item For each record in the TB table, the mean individual weight is computed.

  \item If the species is listed in \code{DataTargetSpecies}, the mean weight is compared to its 5th-95th percentile reference range.

  \item If the mean weight falls outside this interval, a record is saved in a CSV file reporting:
    \itemize{
      \item GSA
      \item Year
      \item Species
      \item Haul number
      \item Observed mean weight
      \item Reference minimum (5th percentile)
      \item Reference maximum (95th percentile)
      \item Type of file
    }
}

\strong{2. Qualitative Check (Plots):}

\itemize{
  \item For all species present in the dataset, the function generates plots of mean weight by haul, to visually check for possible outliers or abnormal variability.

  \item To avoid excessive plotting, only species with at least 5 observations are plotted.

  \item Plots display the mean weight for each haul, with haul numbers shown as labels.

  \item If many species qualify for plotting, the function automatically splits the plots into multiple pages, each saved as a separate JPEG file. Each JPEG file contains up to 6 plots.
}

\strong{Files Produced:}

\itemize{
  \item \emph{Logfile (.dat)}: Contains a short message summarising whether any outliers were detected. Detailed warnings are no longer written here but are instead stored in the CSV file.

  \item \emph{CSV File}: Named as \code{Check_Mean_Weights_Logfile_GSA<AREA>_Year<YEAR>_<suffix>.csv}. Contains all observations where the mean weight falls outside the expected reference range.

  \item \emph{Plots}: Saved in JPEG format in the \code{Graphs} subdirectory. Filenames include GSA, year, and a page number.
}
}
\value{
This function always returns \code{TRUE}. The purpose of the function is to perform checks and report inconsistencies, but it does not interrupt the execution of other routines in the \code{RoME} package.
}
\references{
Anonymous. 2017. MEDITS-Handbook. Version n. 9. MEDITS Working Group. 106 pp. http://www.sibm.it/MEDITS 2011/principaledownload.htm
}
\author{
W. Zupa, I. Bitetto
}
\examples{
TB <- RoME::TB
year <- 2012
wd <- tempdir()
suffix <- "2020-03-05_time_h17m44s55"
check_weight(TB, year, DataTargetSpecies, wd, suffix)
}
\keyword{quality control}
