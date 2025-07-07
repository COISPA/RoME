############################################################################################################################
#   RoME: R code to perform multiple checks on MEDITS Survey data (TA, TB, TC and TE files)                                #
#   Authors: I. Bitetto, W. Zupa, M.T. Spedicato                                                                           #
#   Coispa Tecnologia & Ricerca - Stazione sperimentale per lo Studio delle Risorse del Mare                               #
#   If you have any comments or suggestions please contact the following e-mail address: bitetto@coispa.it, zupa@coispa.it #
#   January 2022                                                                                                           #
############################################################################################################################
#  Check about the consistency of length classes in TC

check_length <- function(DataTC, DataSpecies = NA, year, wd, suffix, DataTargetSpecies = DataTargetSpecies) {

  if (FALSE){
    #library(MEDITS)
    wd <- "D:\\OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L\\RDB3\\test"
    DataSpecies=NA
    suffix=paste(as.character(Sys.Date()),format(Sys.time(), "_time_h%Hm%Ms%OS0"),sep="")
    DataTC = read.table("D:/OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L/______ MEDITS DATA __OFFICIAL___/MEDBSsurvey/Demersal/TC_MEDITS_FORMAT_2025.csv",sep=";",header=TRUE)
    DataTC <- DataTC[DataTC$AREA==25, ]


    wd=tempdir()
    DataTC <- RoME::TC # [1:20,]
    DataSpecies=NA
    suffix= "2020-03-05_time_h17m44s55"
    check_length(DataTC,DataSpecies=NA,year=2015,wd,suffix)

    # check_length(DataTC,DataSpecies=NA,year=2007,wd,suffix)
  }

  if (!file.exists(file.path(wd, "Logfiles"))) {
    dir.create(file.path(wd, "Logfiles"), recursive = TRUE, showWarnings = FALSE)
  }

  if (!exists("suffix")) {
    suffix <- paste(as.character(Sys.Date()), format(Sys.time(), "_time_h%Hm%Ms%OS0"), sep = "")
  }

  # Define paths for log files
  Errors <- file.path(wd, "Logfiles", paste("Logfile_", suffix, ".dat", sep = ""))
  if (!file.exists(Errors)) {
    file.create(Errors)
  }

  AREA <- unique(DataTC$AREA)[1]
  ErrorsCSV <- file.path(
    wd,
    "Logfiles",
    paste("Check_Length_Classes_Logfile_GSA", AREA, "_Year", year, "_", suffix, ".csv", sep = "")
  )

  # Prepare list to collect warnings to write into CSV
  warnings_list <- list()

  # Create CSV header
  csv_header <- data.frame(
    GSA = integer(),
    Year = integer(),
    Species = character(),
    Sex = character(),
    Haul = integer(),
    Length_Class = numeric(),
    LMIN01 = numeric(),
    LMAX99 = numeric(),
    TYPE_OF_FILE = character(),
    Type_of_Warning = character(),
    stringsAsFactors = FALSE
  )

  # Write CSV header
  write.table(
    csv_header,
    file = ErrorsCSV,
    sep = ";",
    row.names = FALSE,
    col.names = TRUE
  )

  # Filter for selected year
  arg <- "year"
  if (!exists(arg)) {
    stop(paste0("'", arg, "' argument should be provided"))
  } else if (length(year) != 1) {
    stop(paste0("only one value should be provided for '", arg, "' argument"))
  } else if (is.na(year)) {
    stop(paste0(arg, " argument should be a numeric value"))
  }

  DataTC <- DataTC[DataTC$YEAR == year, ]

  DataTC <- DataTC[!is.na(DataTC$LENGTH_CLASS), ]

  if (nrow(DataTC) == 0) {
    write("Empty TC data frame for the selected year.", file = Errors, append = TRUE)
  } else {

    Result <- DataTC
    write(
      paste("\n----------- check consistency of length classes TC - ", Result$YEAR[1]),
      file = Errors,
      append = TRUE
    )

    if (all(is.na(DataSpecies))) {
      Target <- RoME::DataTargetSpecies
    } else {
      Target <- DataSpecies
    }

    Target <- Target[which(!is.na(Target$obs_in_TC) & Target$obs_in_TC > 50), ]

    ResultData <- Result[, c("TYPE_OF_FILE", "HAUL_NUMBER", "GENUS", "SPECIES", "SEX", "LENGTH_CLASS")]
    ResultData$species <- paste(ResultData$GENUS, ResultData$SPECIES, sep = "")

    for (i in 1:nrow(ResultData)) {

      # Check for NA or empty LENGTH_CLASS
      if (is.na(ResultData$LENGTH_CLASS[i]) || ResultData$LENGTH_CLASS[i] == "") {
        warnings_list[[length(warnings_list) + 1]] <- data.frame(
          GSA = AREA,
          Year = year,
          Species = ResultData$species[i],
          Sex = ResultData$SEX[i],
          Haul = ResultData$HAUL_NUMBER[i],
          Length_Class = NA,
          LMIN01 = NA,
          LMAX99 = NA,
          TYPE_OF_FILE = ResultData$TYPE_OF_FILE[i],
          Type_of_Warning = "Missing LENGTH_CLASS",
          stringsAsFactors = FALSE
        )
      }

      # Check for negative LENGTH_CLASS
      if (!is.na(ResultData$LENGTH_CLASS[i]) && ResultData$LENGTH_CLASS[i] < 0) {
        warnings_list[[length(warnings_list) + 1]] <- data.frame(
          GSA = AREA,
          Year = year,
          Species = ResultData$species[i],
          Sex = ResultData$SEX[i],
          Haul = ResultData$HAUL_NUMBER[i],
          Length_Class = ResultData$LENGTH_CLASS[i],
          LMIN01 = NA,
          LMAX99 = NA,
          TYPE_OF_FILE = ResultData$TYPE_OF_FILE[i],
          Type_of_Warning = "Negative LENGTH_CLASS",
          stringsAsFactors = FALSE
        )
      }

      # Check if length is out of range
      FoundInTable <- Target[as.character(Target$SPECIES) == as.character(ResultData$species[i]), ]
      FoundInTable <- FoundInTable[!is.na(FoundInTable$LMIN01[1]), ]

      if (nrow(FoundInTable) != 0) {
        if (!is.na(ResultData$LENGTH_CLASS[i])) {
          if (ResultData$LENGTH_CLASS[i] < FoundInTable$LMIN01[1] ||
              ResultData$LENGTH_CLASS[i] > FoundInTable$LMAX99[1]) {

            warnings_list[[length(warnings_list) + 1]] <- data.frame(
              GSA = AREA,
              Year = year,
              Species = ResultData$species[i],
              Sex = ResultData$SEX[i],
              Haul = ResultData$HAUL_NUMBER[i],
              Length_Class = ResultData$LENGTH_CLASS[i],
              LMIN01 = FoundInTable$LMIN01[1],
              LMAX99 = FoundInTable$LMAX99[1],
              TYPE_OF_FILE = ResultData$TYPE_OF_FILE[i],
              Type_of_Warning = "LENGTH_CLASS out of reference range",
              stringsAsFactors = FALSE
            )
          }
        }
      }
    }
  }

  # Write all warnings to CSV
  if (length(warnings_list) > 0) {
    warnings_df <- do.call(rbind, warnings_list)
    warnings_df <- warnings_df[order(warnings_df$Species, warnings_df$Haul, warnings_df$Sex), ]
    write.table(
      warnings_df,
      file = ErrorsCSV,
      sep = ";",
      row.names = FALSE,
      col.names = FALSE,
      append = TRUE
    )

    # Write summary to log
    write(
      paste(
        nrow(warnings_df), "rows written to CSV file",
        basename(ErrorsCSV),
        "- possible inconsistencies detected in LENGTH_CLASS values."
      ),
      file = Errors,
      append = TRUE
    )
  } else {
    write("No inconsistencies detected. All length classes within expected ranges.", file = Errors, append = TRUE)
  }

  # Function always returns TRUE
  return(TRUE)
}
