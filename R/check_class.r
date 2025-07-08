

check_class <- function(data, tab, suffix, wd) {

  # Create log folder if it doesn't exist
  if (!file.exists(file.path(wd, "Logfiles"))) {
    dir.create(file.path(wd, "Logfiles"), recursive = TRUE, showWarnings = FALSE)
  }

  numberError <- 0

  if (!exists("suffix")) {
    suffix <- paste(
      as.character(Sys.Date()),
      format(Sys.time(), "_time_h%Hm%Ms%OS0"),
      sep = ""
    )
  }

  Errors <- file.path(wd, "Logfiles", paste("Logfile_", suffix, ".dat", sep = ""))
  Critical_Errors <- file.path(wd, paste("Critical_errors_", suffix, ".dat", sep = ""))

  if (!file.exists(Errors)) {
    file.create(Errors)
  }

  # Select the correct template based on the table name
  if (tab == "TA") template <- RoME::templateTA
  if (tab == "TB") template <- RoME::templateTB
  if (tab == "TC") template <- RoME::templateTC
  if (tab == "TE") template <- RoME::templateTE
  if (tab == "TL") template <- RoME::templateTL

  ncols <- ncol(data)

  if (ncols == ncol(template)) {

    # Load classes table from package
    classes <- RoME::classes

    # Check numeric fields
    numerics <- classes[classes$type %in% c("numeric"), ]

    for (i in seq_len(ncols)) {
      col_name <- colnames(data)[i]

      if (col_name %in% numerics[numerics$table == tab, "MEDITS"]) {
        index <- which(numerics$MEDITS == col_name & numerics$table == tab)
      } else {
        index <- NA
      }

      if (!is.na(index)) {
        if (!is.numeric(data[[col_name]])) {
          msg <- paste(
            "The class (", class(data[[col_name]]),
            ") of column ", col_name,
            " in ", tab,
            " is inconsistent with the expected one (", numerics[index, "type"], ")"
          )
          write(msg, file = Errors, append = TRUE)
          numberError <- numberError + 1

          if (!file.exists(Critical_Errors)) {
            file.create(Critical_Errors)
          }
          write(msg, file = Critical_Errors, append = TRUE)
        }
      }
    }

    # Check integer fields
    integers <- classes[classes$type %in% c("integer"), ]

    for (i in seq_len(ncols)) {
      col_name <- colnames(data)[i]

      if (col_name %in% integers[integers$table == tab, "MEDITS"]) {
        index <- which(integers$MEDITS == col_name & integers$table == tab)
      } else {
        index <- NA
      }

      if (!is.na(index)) {
        if (!is.integer(data[[col_name]])) {
          msg <- paste(
            "The class (", class(data[[col_name]]),
            ") of column ", col_name,
            " in ", tab,
            " is inconsistent with the expected one (", integers[index, "type"], ")"
          )
          write(msg, file = Errors, append = TRUE)
          numberError <- numberError + 1
        }
      }
    }
  }

  if (numberError == 0) {
    write("No error occurred", file = Errors, append = TRUE)
    return(TRUE)
  } else {
    return(FALSE)
  }
}
