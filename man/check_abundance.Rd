\name{check_abundance}
\alias{check_abundance}
\title{Box-plots of swept-area-standardised abundance indices}
\description{
This function reads matching MEDITS \code{TA} (haul metadata) and
\code{TB} (catch numbers) tables for a single survey year, computes the swept
area for each haul, derives haul-level abundance indices expressed as
individuals per km\(^2\), and produces one or more box-plots that summarise the
distribution of those indices by species.  The plots are written to the folder
\file{<wd>/Graphs/Abundance/} and a logfile is produced under
\file{<wd>/Logfiles/} with a summary of any quality-control issues
encountered.
}
\usage{
check_abundance(ResultDataTA, ResultDataTB, year, wd,
suffix = NULL, distance_unit = "km", min_hauls = 5L)
}
\arguments{
  \item{ResultDataTA}{Data frame containing at least the columns \code{COUNTRY},
    \code{YEAR}, \code{MONTH}, \code{DAY}, \code{HAUL_NUMBER}, \code{DISTANCE}
    (towed length) and \code{WING_OPENING} (gear width in metres).}
  \item{ResultDataTB}{Data frame containing at least \code{COUNTRY}, \code{YEAR},
    \code{MONTH}, \code{DAY}, \code{HAUL_NUMBER}, \code{GENUS}, \code{SPECIES},
    \code{TOTAL_NUMBER_IN_THE_HAUL} and \code{FAUNISTIC_CATEGORY}.}
  \item{year}{Single numeric value identifying the survey year to analyse.}
  \item{wd}{Writable directory where the function will place \file{Logfiles/}
    and \file{Graphs/Abundance/}.}
  \item{suffix}{Optional text appended to the logfile name.  When missing a
    timestamp is used.}
  \item{distance_unit}{Either \code{"km"} (default) or \code{"nmi"}.  The
    value determines the conversion factor applied to \code{DISTANCE}.}
  \item{min_hauls}{Minimum number of valid hauls a species must appear in to be
    plotted.  Defaults to \code{5}.}
}
\details{
A swept area is first computed for every haul from \code{TA}.  The function
then merges \code{TA} and \code{TB}.  Hauls that lack swept area information
or hauls whose total number of individuals is missing (or equal to zero when
not allowed by the faunistic category) are discarded and reported in the
logfile.  Species occurring in fewer than \code{min_hauls} valid hauls are also
removed.  The remaining species are sorted alphabetically and divided into
blocks of 36; one image file is produced for each block so that labels remain
readable even in years with many species.

The function returns \code{TRUE} when no data-quality issues were found and
\code{FALSE} otherwise.
}
\value{
Logical scalar (\code{TRUE} if no errors, \code{FALSE} otherwise).
}
\author{W. Zupa.}
\examples{
\dontrun{
wd <- tempdir()
check_abundance(ResultDataTA = RoME::TA,
               ResultDataTA = RoME::TB,
               year = 2007,
               wd   = wd)
}
}
