\name{RoMEBScc}
\alias{RoMEBScc}
\title{Continuous Quality Checks on Black Sea trawl Survey Data}
\description{
The function \code{RoMEBScc} performs a full suite of automated, non-stopping quality checks on survey data for the Black Sea.  It invokes the same sequence of validation routines as \code{RoMEcc} (for TA, TB, TC, TE and TL tables) but uses tables and parameters tailored to Black Sea strata, species and maturity references.
}
\usage{
RoMEBScc(
  TA,
  TB,
  TC,
  TE = NA,
  TL = NA,
  wd,
  suffix = NA,
  verbose = TRUE,
  Stratification = RoME::stratification_scheme,
  Ref_list = RoME::TM_list,
  DataTargetSpecies = RoME::DataTargetSpecies,
  Maturity = RoME::Maturity_parameters,
  ab_parameters = RoME::LW,
  stages_list = RoME::mat_stages,
  assTL = RoME::assTL,
  zip = TRUE
)
}
\arguments{
  \item{TA}{Haul metadata table (TA)}
  \item{TB}{Catch data table (TB)}
  \item{TC}{Biological data table (TC)}
  \item{TE}{Individual biological data table (TE), or \code{NA} if unused.}
  \item{TL}{Litter table (TL), or \code{NA} if unused.}
  \item{wd}{Working directory path where "Logfiles" and "Graphs" subfolders are written.}
  \item{suffix}{Character suffix for output filenames.  If \code{NA}, a timestamp is auto-generated.}
  \item{verbose}{Logical; if \code{TRUE}, progress messages are printed to the console.}
  \item{Stratification}{Stratification scheme object; defaults to \code{RoME::stratification_scheme}.}
  \item{Ref_list}{Reference list for tow metadata; defaults to \code{RoME::TM_list}.}
  \item{DataTargetSpecies}{Species-specific reference dataset; defaults to \code{RoME::DataTargetSpecies}.}
  \item{Maturity}{Maturity-parameter table; defaults to \code{RoME::Maturity_parameters}.}
  \item{ab_parameters}{Length-weight parameter table; defaults to \code{RoME::LW}.}
  \item{stages_list}{Maturity-stage lookup table; defaults to \code{RoME::mat_stages}.}
  \item{assTL}{Category-subcategory mapping for TL; defaults to \code{RoME::assTL}.}
  \item{zip}{Logical; if \code{TRUE}, a ZIP archive of results is created at the end.}
}
\details{
\code{RoMEBScc} checks can be used to integrate a list of common quality checks on survey datafrom Black Sea. This function calls all the functions built in the package in an ordered way to perform a complete quality check of TX data available. The order of the checks in \code{RoME} was implemented in a defined sequence to avoid cascade errors due to the correction of a previous error. No automatic correction is implemented in 'RoME'. 'RoME' stops if an error occurs; then the user has to correct the error and run again the code to continue with the other checks. The function runs on a complete time series dataset, checking year after year, until the end of the time series. After the checks of the mandatory fields and the controlled vocabulary, that are carried out for all the TX tables, the specific checks on each kind of TX table are performed. Finally, \code{RoME} provides a list of cross checks aimed to guarantee the consistency among the data tables.

Some functions included in the 'RoME' library and used by \code{RoME} function need specific dictionaries or tables. It is the case of \code{Stratification}, \code{TM_list}, \code{DataTargetSpecies}, \code{Maturity_parameters}, \code{mat_stages} and \code{assTL} tables. All of them are provided by default in this library. Anyway, the user has the possibility to provide \emph{ad hoc} modified versions of these tables adapting the checks to specific needs.
}
\value{
The function does not correct data, but it detects the errors, warning the user that there is the possibility of one or more errors, specifying the type of the error and easing the data correction. If parameter \code{verbose=TRUE} returns a series of text output in console to let the user to trace the state of the checks. All the output of the functions are stored in the user defined working directory  \code{wd} and in the sub-directory there resident. In the Lofile subfolder are stored the logfiles of each run of the function.
}
\references{Anonymus. 2017. MEDITS-Handbook. Version n. 9. MEDITS Working Group. 106 pp. https://www.sibm.it/MEDITS 2011/principaledownload.htm}
\author{W. Zupa}
\examples{
\donttest{
wd <- tempdir()
# Example using TA, TB, TC slices for 2018
TA_sub <- RoME::TA[RoME::TA$YEAR == 2018, ]
TB_sub <- RoME::TB[RoME::TB$YEAR == 2018, ]
TC_sub <- RoME::TC[RoME::TC$YEAR == 2018, ]
# TE/TL not used in this example
RoMEBScc(
  TA_sub, TB_sub, TC_sub, TE = NA, TL = NA,
  wd = wd, suffix = "BS_test",
  verbose = FALSE
)
}}
\keyword{survey}
\keyword{quality}
\keyword{BlackSea}
