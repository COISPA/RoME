if (FALSE){
  ResultData = read.table("D:/OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L/______ MEDITS DATA __OFFICIAL___/MEDBSsurvey/Demersal/TC_MEDITS_FORMAT_2025.csv",sep=";",header=TRUE)
  ResultData <- ResultData[ResultData$AREA == 18 & ResultData$YEAR == 2023, ]
  year=2023
  MaturityParameters=RoME::Maturity_parameters
  TargetSpecies=RoME::DataTargetSpecies
  wd <- "D:\\OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L\\RDB3\\test"
  # suffix="smallest mature"
  suffix=paste(as.character(Sys.Date()),format(Sys.time(), "_time_h%Hm%Ms%OS0"),sep="")
  check_smallest_mature(ResultData,year,MaturityParameters=RoME::Maturity_parameters,TargetSpecies=RoME::DataTargetSpecies,wd,suffix)
}


check_smallest_mature <- function(
    ResultData,
    year,
    MaturityParameters = RoME::Maturity_parameters,
    TargetSpecies = RoME::DataTargetSpecies,
    wd,
    suffix
) {
  if (!file.exists(file.path(wd, "Logfiles"))) {
    dir.create(file.path(wd, "Logfiles"), recursive = TRUE, showWarnings = FALSE)
  }

  if (!exists("suffix")) {
    suffix <- paste(as.character(Sys.Date()), format(Sys.time(), "_time_h%Hm%Ms%OS0"), sep = "")
  }

  AREA <- unique(ResultData$AREA)[1]

  # Define paths for log files
  Errors <- file.path(wd,"Logfiles",paste("Logfile_", suffix ,".dat",sep=""))
  if (!file.exists(Errors)){
    file.create(Errors)
  }


  ErrorsCSV <- file.path(
    wd, "Logfiles",
    paste("Check_Smallest_Mature_Logfile_GSA", AREA, "_Year", year, "_", suffix, ".csv", sep = "")
  )

  # Prepare CSV header
  csv_header <- data.frame(
    GSA = integer(),
    Year = integer(),
    Species = character(),
    Sex = character(),
    Haul = integer(),
    Length_Class_mm = numeric(),
    Threshold_mm = numeric(),
    Literature_Threshold_mm = numeric(),
    Reference = character(),
    TYPE_OF_FILE = character(),
    Type_of_Warning = character(),
    stringsAsFactors = FALSE
  )

  write.table(
    csv_header,
    file = ErrorsCSV,
    sep = ";",
    row.names = FALSE,
    col.names = TRUE
  )

  write(
    paste("\n----------- check smallest mature individuals in TC -", year),
    file = Errors,
    append = TRUE
  )

  # Filter ResultData for the selected year
  ResultData <- ResultData[ResultData$YEAR == year, ]
  ResultData <- ResultData[!is.na(ResultData$LENGTH_CLASS), ]

  if (nrow(ResultData) == 0) {
    write("Empty TC data frame for the selected year.", file = Errors, append = TRUE)
    return(TRUE)
  }

  # Prepare maturity table
  mat_lmin <- MaturityParameters
  mat_lmin$smallest_mature_individual_observed <- as.numeric(
    mat_lmin$smallest_mature_individual_observed
  )
  mat_lmin <- mat_lmin[!is.na(mat_lmin$smallest_mature_individual_observed), ]
  mat_lmin$Species <- as.character(mat_lmin$Species)

  # Prepare species list
  species_list <- TargetSpecies
  species_list$FAUNISTIC_CATEGORY <- as.character(species_list$FAUNISTIC_CATEGORY)

  ResultData$Species <- paste(ResultData$GENUS, ResultData$SPECIES)
  ResultData$Maturity <- paste(
    as.character(ResultData$MATURITY),
    ifelse(is.na(ResultData$MATSUB), "", as.character(ResultData$MATSUB)),
    sep = ""
  )

  # Define immature codes
  immature_codes <- c("0", "1", "2A")

  warnings_list <- list()

  # Loop over all species present in maturity parameters
  for (i in unique(mat_lmin$Species)) {
    cau_fau_temp <- species_list$FAUNISTIC_CATEGORY[
      paste0(
        substring(species_list$SPECIES, 1, 4),
        substring(species_list$SPECIES, 5, 7)
      ) == i
    ]

    mat_lmin_temp <- mat_lmin[mat_lmin$Species == i, ]

    for (j in 1:nrow(mat_lmin_temp)) {

      # Select records for this species (and sex if not combined)
      if (as.character(mat_lmin_temp$SEX[j]) == "C") {
        ResultData_temp <- ResultData[
          ResultData$Species == i,
        ]
        mat_lmin_temp_sex <- mat_lmin_temp[mat_lmin_temp$SEX == "C", ]
      } else {
        ResultData_temp <- ResultData[
          ResultData$Species == i &
            ResultData$SEX == as.character(mat_lmin_temp$SEX[j]),
        ]
        mat_lmin_temp_sex <- mat_lmin_temp[
          mat_lmin_temp$SEX == as.character(mat_lmin_temp$SEX[j]),
        ]
      }

      # Bibliographic threshold (cm) from MaturityParameters
      literature_cm <- mat_lmin_temp_sex$smallest_mature_individual_observed[1]

      # Convert thresholds to mm
      literature_mm <- literature_cm * 10
      threshold_mm <- (literature_cm - 0.1 * literature_cm) * 10

      # Check for mature individuals below threshold
      Error_matrix <- ResultData_temp[
        !ResultData_temp$Maturity %in% immature_codes &
          ResultData_temp$LENGTH_CLASS < threshold_mm,
      ]

      if (nrow(Error_matrix) != 0) {
        for (k in 1:nrow(Error_matrix)) {
          warnings_list[[length(warnings_list) + 1]] <- data.frame(
            GSA = AREA,
            Year = year,
            Species = Error_matrix$Species[k],
            Sex = Error_matrix$SEX[k],
            Haul = Error_matrix$HAUL_NUMBER[k],
            Length_Class_mm = Error_matrix$LENGTH_CLASS[k],
            Threshold_mm = threshold_mm,
            Literature_Threshold_mm = literature_mm,
            Reference = mat_lmin_temp_sex$Reference[1],
            TYPE_OF_FILE = Error_matrix$TYPE_OF_FILE[k],
            Type_of_Warning = paste(
              "Mature individual below bibliographic threshold of",
              literature_mm, "mm",
              "- threshold applied with 10% buffer =", round(threshold_mm, 1), "mm"
            ),
            stringsAsFactors = FALSE
          )
        }
      }
    }
  }

  # Write results to CSV
  if (length(warnings_list) > 0) {
    warnings_df <- do.call(rbind, warnings_list)
    warnings_df <- warnings_df[
      order(warnings_df$Species, warnings_df$Haul, warnings_df$Sex),
    ]

    write.table(
      warnings_df,
      file = ErrorsCSV,
      sep = ";",
      row.names = FALSE,
      col.names = FALSE,
      append = TRUE
    )

    write(
      paste(
        nrow(warnings_df), "rows written to CSV file",
        basename(ErrorsCSV),
        "- records found with mature individuals below threshold."
      ),
      file = Errors,
      append = TRUE
    )
  } else {
    write(
      "No inconsistencies detected. All maturity stages above bibliographic thresholds.",
      file = Errors,
      append = TRUE
    )
  }

  write(
    paste("Attention: if you decide to change the maturity stages detected,",
          "after the corrections, run again the code,",
          "because you could have entered duplicated records in TC."),
    file = Errors,
    append = TRUE
  )

  return(TRUE)
}
