\name{check_raising}
\alias{check_raising}
\title{Comprehensive consistency checks between MEDITS \code{TB} and \code{TC} files}
\description{
\code{check_raising()} performs a suite of integrity checks on a pair of MEDITS
survey tables -- \emph{TB} (catch-at-haul) and \emph{TC} (biological subsamples) -- for
one survey year. All inconsistencies are written to a plain-text logfile; the
routine never stops at the first error, so you always get the full list of
issues detected.
}
\usage{
check_raising(ResultDataTB, ResultDataTC, year, wd, suffix = NULL)
}
\arguments{
\item{ResultDataTB}{A \code{data.frame} containing the MEDITS \strong{TB}
table for one or more years.}
\item{ResultDataTC}{A \code{data.frame} containing the MEDITS \strong{TC}
table for one or more years.}
\item{year}{Single integer. The survey year to be checked.}
\item{wd}{Character string. A writable directory where sub-folders
\file{Logfiles/} (and \file{Graphs/}, currently unused) will be created.}
\item{suffix}{Optional character string appended to the logfile name. When
\code{NULL} (default) a timestamp-based suffix is generated automatically.}
}
\details{
The function executes \strong{five} independent validations:
\enumerate{
\item \emph{Weight consistency} -- When more than one subsample exists for a
given haul/species in \code{TC}, the sum of
\code{WEIGHT_OF_THE_FRACTION} must equal \code{TOTAL_WEIGHT_IN_THE_HAUL}
recorded in \code{TB}.
\item \emph{Raising factor} -- For each subsample the ratio \eqn{molt}{molt} given by \code{WEIGHT_OF_THE_FRACTION} / \code{WEIGHT_OF_THE_SAMPLE_MEASURED} must be \eqn{\ge 1}{\ge 1}.
\item \emph{Sex-specific numbers} -- For every combination
haul \code{*} species \code{*} sex the raised total of individuals in \code{TC}
must match \code{TB} columns \code{NB_OF_FEMALES}, \code{NB_OF_MALES}
or \code{NB_OF_UNDETERMINED}.
\item \emph{Total individuals (TC -> TB)} -- For every haul/species the sum of
raised numbers across all sexes in \code{TC} must equal
\code{TOTAL_NUMBER_IN_THE_HAUL} in \code{TB}. This catches cases where
an entire sex is missing from \code{TC}.
\item \emph{Internal TB consistency} -- Within \code{TB} the sum of the three
sex-specific columns must equal \code{TOTAL_NUMBER_IN_THE_HAUL} for each
haul/species.
}

If any of these checks fails, an explanatory line is appended to the logfile
\file{Logfiles/Logfile_.dat}. The function finally returns a single
logical value: \code{TRUE} when no errors are detected, \code{FALSE} otherwise.

Tidy-evaluation is used inside the \pkg{dplyr} pipelines; the following symbols
are declared in \link[utils]{globalVariables} to avoid "no visible binding"
notes during \command{R CMD check}:
\code{COUNTRY}, \code{YEAR}, \code{HAUL_NUMBER}, \code{GENUS}, \code{SPECIES},
\code{WEIGHT_OF_THE_FRACTION}, \code{WEIGHT_OF_THE_SAMPLE_MEASURED},
\code{NUMBER_OF_INDIVIDUALS_IN_THE_LENGTH_CLASS_AND_MATURITY_STAGE}, \code{SEX},
\code{NB_OF_FEMALES}, \code{NB_OF_MALES}, \code{NB_OF_UNDETERMINED},
\code{TOTAL_WEIGHT_IN_THE_HAUL}, \code{TOTAL_NUMBER_IN_THE_HAUL},
\code{codedsex}, \code{N}, \code{molt}, \code{raised}, \code{RaisedSex},
\code{RaisedTotal}, \code{n_subsamples}, \code{total_fraction},
\code{SumSexTB}.
}
\value{
Logical scalar: \code{TRUE} if the dataset passes all checks, \code{FALSE}
otherwise.
}
\author{I. Bitetto, W. Zupa, M. T. Spedicato}
\seealso{\code{\link[dplyr]{dplyr}} for the data-manipulation verbs used under
the hood.}
\examples{
\donttest{

# The following datasets come from the 'RoME' package demo data
DataTB <- RoME::TB
DataTC <- RoME::TC

res <- check_raising(DataTB, DataTC, year = 2015, wd = tempdir())
if (res) message("All checks passed!")
}
}



