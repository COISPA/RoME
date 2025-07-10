
# Check if the values in a given field belong to allowed ranges (INSTRUCTION MANUAL MEDITS 2017)
check_dictionary <- function(
    ResultData,    # data.frame with survey data
    Field,         # character: name of the column to check
    Values,        # character vector: allowed values (can include NA)
    year,          # numeric: year to filter
    wd,            # working directory for logs
    suffix         # character: suffix for logfile
) {
  # --- Setup log directory and logfile ---
  log_dir <- file.path(wd, "Logfiles")
  if (!dir.exists(log_dir)) dir.create(log_dir, recursive = TRUE)
  if (missing(suffix) || is.na(suffix)) {
    suffix <- paste0(Sys.Date(), format(Sys.time(), "_time_h%Hm%Ms%OS0"))
  }
  log_file <- file.path(log_dir, paste0("Logfile_", suffix, ".dat"))
  if (!file.exists(log_file)) file.create(log_file)

  # --- Header for this check ---
  write(paste0("\n----------- check dictionary for field: ", Field, " - ", year),
        file = log_file, append = TRUE)

  # --- Filter data for the specified year ---
  if (length(year) != 1 || is.na(year)) {
    stop("Argument 'year' must be a single non-NA value")
  }
  df <- ResultData[ResultData$YEAR == year, , drop = FALSE]
  tf <- if (nrow(df) > 0) df$TYPE_OF_FILE[1] else NA_character_

  # --- Prepare values ---
  all_vals <- as.character(df[[Field]])
  Values_char <- as.character(Values)
  allowsNA <- any(is.na(Values))
  allowed_vals <- setdiff(Values_char, NA_character_)

  # --- 1) NA entries not allowed ---
  msgs_na <- character()
  if (!allowsNA) {
    na_idx <- which(is.na(all_vals))
    if (length(na_idx) > 0) {
      msgs_na <- sprintf(
        "Haul %s: value not allowed for %s in %s (actual: NA)",
        df$HAUL_NUMBER[na_idx], Field, tf
      )
    }
  }

  # --- 2) Empty strings ---
  empty_idx <- which(!is.na(all_vals) & all_vals == "")
  msgs_empty <- character()
  if (length(empty_idx) > 0) {
    msgs_empty <- switch(tf,
                         TA = sprintf("Haul %s: the field %s is empty in %s", df$HAUL_NUMBER[empty_idx], Field, tf),
                         TB = sprintf("Haul %s %s %s: the field %s is empty in %s", df$HAUL_NUMBER[empty_idx], df$GENUS[empty_idx], df$SPECIES[empty_idx], Field, tf),
                         TC = sprintf("Haul %s %s %s %s %s: the field %s is empty in %s", df$HAUL_NUMBER[empty_idx], df$GENUS[empty_idx], df$SPECIES[empty_idx], df$SEX[empty_idx], df$LENGTH_CLASS[empty_idx], Field, tf),
                         TE = sprintf("Haul %s %s %s %s %s: the field %s is empty in %s", df$HAUL_NUMBER[empty_idx], df$GENUS[empty_idx], df$SPECIES[empty_idx], df$SEX[empty_idx], df$LENGTH_CLASS[empty_idx], Field, tf),
                         TL = sprintf("Haul %s %s %s %s %s: the field %s is empty in %s", df$HAUL_NUMBER[empty_idx], df$GENUS[empty_idx], df$SPECIES[empty_idx], df$SEX[empty_idx], df$LENGTH_CLASS[empty_idx], Field, tf),
                         character()
    )
  }

  # --- 3) Invalid values ---
  inv_idx <- which(!is.na(all_vals) & all_vals != "" & !(all_vals %in% allowed_vals))
  if (Field == "SEX") inv_idx <- inv_idx[all_vals[inv_idx] != "FALSE"]
  msgs_inv <- character()
  if (length(inv_idx) > 0) {
    msgs_inv <- switch(tf,
                       TA = sprintf("Haul %s: value '%s' not allowed for %s in %s", df$HAUL_NUMBER[inv_idx], all_vals[inv_idx], Field, tf),
                       TB = sprintf("Haul %s %s %s: value '%s' not allowed for %s in %s", df$HAUL_NUMBER[inv_idx], df$GENUS[inv_idx], df$SPECIES[inv_idx], all_vals[inv_idx], Field, tf),
                       TC = sprintf("Haul %s %s %s %s %s: value '%s' not allowed for %s in %s", df$HAUL_NUMBER[inv_idx], df$GENUS[inv_idx], df$SPECIES[inv_idx], df$SEX[inv_idx], df$LENGTH_CLASS[inv_idx], all_vals[inv_idx], Field, tf),
                       TE = sprintf("Haul %s %s %s %s %s: value '%s' not allowed for %s in %s", df$HAUL_NUMBER[inv_idx], df$GENUS[inv_idx], df$SPECIES[inv_idx], df$SEX[inv_idx], df$LENGTH_CLASS[inv_idx], all_vals[inv_idx], Field, tf),
                       TL = sprintf("Haul %s: value '%s' not allowed for %s in %s", df$HAUL_NUMBER[inv_idx], all_vals[inv_idx], Field, tf),
                       character()
    )
  }

  # --- Combine messages and write to log ---
  msgs <- c(msgs_na, msgs_empty, msgs_inv)
  if (length(msgs) == 0) msgs <- sprintf("No error occurred for field %s in %s", Field, tf)
  writeLines(msgs, log_file, sep = "\n")

  # --- Return TRUE if no errors ---
  length(msgs) == 1 && grepl("^No error occurred", msgs)
}
