#' Check consistency of length classes in TC table
#'
#' Filters the TC data for a given year and validates the LENGTH_CLASS field
#' against species-specific reference limits. It reports any missing,
#' negative, or out-of-range values by writing a single CSV of warnings and
#' appending a concise summary to the log file.
#'
#' @param DataTC A \code{data.frame} of TC (catch) records formatted per MEDITS.
#' @param DataSpecies Optional \code{data.frame} of species-level reference limits;
#'   if \code{NA}, the internal \code{DataTargetSpecies} dataset is used.
#' @param year Integer. Year to select from \code{DataTC}.
#' @param wd Character. Directory in which the \code{Logfiles} folder,
#'   the .dat log, and the CSV of warnings will be created.
#' @param suffix Character. Unique suffix applied to log and CSV filenames.
#' @param DataTargetSpecies Data.frame of default species reference limits;
#'   defaults to \code{RoME::DataTargetSpecies}.
#'
#' @details
#' This function first ensures that the log directory and output files
#' exist. It then subsets \code{DataTC} to the specified year and non-missing
#' length classes. A reference table (either \code{DataSpecies} or
#' \code{DataTargetSpecies}) is filtered to species with sufficient observations,
#' and joined by key to obtain \code{LMIN01} and \code{LMAX99}. The function
#' identifies three kinds of issues-missing, negative, and out-of-range
#' length classes-collects all warnings in one data frame, and writes them
#' to CSV in a single batch. Finally, it appends a one-line summary to the
#' logfile indicating how many rows were written or that no inconsistencies
#' were found.
#'
#' @return Invisibly returns \code{TRUE}. All side-effects (CSV and log writes)
#'   occur within the working directory.
#'
#' @author W. Zupa, I. Bitetto; optimized by [Your Name]
#' @seealso \code{\link{check_dictionary}}, \code{\link{RoMEcc}}
#' @keywords validation, data-cleaning, MEDITS
#'
#' @examples
#' wd <- tempdir()
#' sample_TC <- head(RoME::TC, 20)
#' suffix <- "2025-07-10_test"
#' check_length(sample_TC, DataSpecies = NA, year = 2007, wd, suffix)
#' importFrom(dplyr, filter, mutate)
#' importFrom(data.table, setDT)
#' @export


check_length <- function(
    DataTC,
    DataSpecies = NA,
    year,
    wd,
    suffix,
    DataTargetSpecies = RoME::DataTargetSpecies
) {
  # Select internal reference if DataSpecies is NA
  if (length(DataSpecies) == 1 && is.na(DataSpecies)) {
    DataSpecies <- DataTargetSpecies
  }

  # Setup log directory and logfile
  log_dir <- file.path(wd, "Logfiles")
  if (!dir.exists(log_dir)) dir.create(log_dir, recursive = TRUE)
  if (missing(suffix) || is.na(suffix)) {
    suffix <- paste0(Sys.Date(), format(Sys.time(), "_time_h%Hm%Ms%OS0"))
  }
  log_file <- file.path(log_dir, paste0("Logfile_", suffix, ".dat"))
  if (!file.exists(log_file)) file.create(log_file)

  AREA <- unique(DataTC$AREA)[1]
  csv_file <- file.path(
    log_dir,
    paste0("Check_Length_Classes_Logfile_GSA", AREA,
           "_Year", year, "_", suffix, ".csv")
  )

  # Write CSV header
  header <- data.frame(
    GSA=integer(), Year=integer(), Species=character(), Sex=character(),
    Haul=integer(), Length_Class=numeric(), LMIN01=numeric(), LMAX99=numeric(),
    TYPE_OF_FILE=character(), Type_of_Warning=character(), stringsAsFactors=FALSE
  )
  write.table(header, file=csv_file, sep=";", row.names=FALSE, col.names=TRUE)

  # Filter and prepare data
  if (length(year) != 1 || is.na(year)) {
    stop("Argument 'year' must be a single non-NA value.")
  }
  df <- DataTC[DataTC$YEAR == year & !is.na(DataTC$LENGTH_CLASS), , drop = FALSE]
  # If no data for year, log and exit
  if (nrow(df) == 0) {
    write("Empty TC data frame for the selected year.", file = log_file, append = TRUE)
    return(TRUE)
  }
  write(paste0("\n----------- check consistency of length classes TC - ", year),
        file=log_file, append=TRUE)

  # Prepare reference limits
  ref <- DataSpecies[!is.na(DataSpecies$obs_in_TC) & DataSpecies$obs_in_TC > 50, ]
  df$species <- paste0(df$GENUS, df$SPECIES)
  idx_ref <- match(df$species, as.character(ref$SPECIES))
  LMIN <- ref$LMIN01[idx_ref]
  LMAX <- ref$LMAX99[idx_ref]

  # Identify indices for warnings
  missing_idx <- which(is.na(df$LENGTH_CLASS) | df$LENGTH_CLASS == "")
  neg_idx <- which(!is.na(df$LENGTH_CLASS) & df$LENGTH_CLASS < 0)
  valid_ref <- which(!is.na(LMIN) & !is.na(LMAX))
  out_idx <- valid_ref[ df$LENGTH_CLASS[valid_ref] < LMIN[valid_ref] |
                          df$LENGTH_CLASS[valid_ref] > LMAX[valid_ref] ]

  # Build warning data frame only if there are any indices
  warn_idx <- c(missing_idx, neg_idx, out_idx)
  if (length(warn_idx) > 0) {
    warn_df <- data.frame(
      GSA = AREA,
      Year = year,
      Species = df$species[warn_idx],
      Sex = df$SEX[warn_idx],
      Haul = df$HAUL_NUMBER[warn_idx],
      Length_Class = df$LENGTH_CLASS[warn_idx],
      LMIN01 = c(rep(NA, length(missing_idx) + length(neg_idx)), LMIN[out_idx]),
      LMAX99 = c(rep(NA, length(missing_idx) + length(neg_idx)), LMAX[out_idx]),
      TYPE_OF_FILE = df$TYPE_OF_FILE[warn_idx],
      Type_of_Warning = c(
        rep("Missing LENGTH_CLASS", length(missing_idx)),
        rep("Negative LENGTH_CLASS", length(neg_idx)),
        rep("LENGTH_CLASS out of reference range", length(out_idx))
      ),
      stringsAsFactors = FALSE
    )

    # Write warnings and summary when present
    warn_df <- warn_df[order(warn_df$Species, warn_df$Haul, warn_df$Sex), ]
    write.table(
      warn_df,
      file = csv_file,
      sep = ";",
      row.names = FALSE,
      col.names = FALSE,
      append = TRUE
    )
    write(
      paste0(nrow(warn_df), " rows written to CSV file ", basename(csv_file),
             " - possible inconsistencies detected."),
      file = log_file,
      append = TRUE
    )
  } else {
    # No warnings: write a brief summary
    write(
      "No inconsistencies detected. All length classes within expected ranges.",
      file = log_file,
      append = TRUE
    )
  }

  return(TRUE)
}

