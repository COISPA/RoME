# Check maturity stages using spawning season

if (FALSE){
  ResultDataTC = read.table("D:/OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L/______ MEDITS DATA __OFFICIAL___/MEDBSsurvey/Demersal/TC_MEDITS_FORMAT_2025.csv",sep=";",header=TRUE)
  ResultDataTA = read.table("D:/OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L/______ MEDITS DATA __OFFICIAL___/MEDBSsurvey/Demersal/TA_MEDITS_FORMAT_2025.csv",sep=";",header=TRUE)
  ResultDataTC <- ResultDataTC[ResultDataTC$AREA == 18, ]
  ResultDataTA <- ResultDataTA[ResultDataTA$AREA == 18, ]
  year=2023
  wd <- "D:\\OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L\\RDB3\\test"
  suffix="NA"
  # load("C:/Users/Bitetto Isabella/OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L/Rome/ROME//RoME//data//DataTargetSpecies.rda")
  # load("C:/Users/Bitetto Isabella/OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L/Rome/ROME//RoME//data//Maturity_parameters.rda")
  check_spawning_period(ResultDataTA,ResultDataTC,year,Maturity_parameters=Maturity_parameters,DataTargetSpecies=DataTargetSpecies,wd,suffix)
}

check_spawning_period <- function(
    ResultDataTA,
    ResultDataTC,
    year,
    Maturity_parameters = Maturity_parameters,
    DataTargetSpecies = DataTargetSpecies,
    wd,
    suffix) {

  # Safety
  if (!file.exists(file.path(wd, "Logfiles"))) {
    dir.create(file.path(wd, "Logfiles"), recursive = TRUE, showWarnings = FALSE)
  }
  if (!exists("suffix")) {
    suffix <- paste(as.character(Sys.Date()), format(Sys.time(), "_time_h%Hm%Ms%OS0"), sep = "")
  }

  numberError <- 0

  # Logfile
  Errors <- file.path(wd, "Logfiles", paste("Logfile_", suffix, ".dat", sep = ""))
  if (!file.exists(Errors)) {
    file.create(Errors)
  }

  # CSV output
  ErrorsCSV <- file.path(
    wd, "Logfiles",
    paste("Check_SpawningPeriod_Logfile_GSA", unique(ResultDataTC$AREA)[1], "_Year", year, "_", suffix, ".csv", sep = "")
  )

  csv_header <- data.frame(
    GSA = integer(),
    Year = integer(),
    Haul = integer(),
    Species = character(),
    Sex = character(),
    Length_Class_mm = numeric(),
    Maturity_Stage = character(),
    Start_Spawning_Month = integer(),
    End_Spawning_Month = integer(),
    Month_of_Catch = integer(),
    Threshold_mm = numeric(),
    Warning_Type = character(),
    stringsAsFactors = FALSE
  )

  write.table(
    csv_header,
    file = ErrorsCSV,
    sep = ";",
    row.names = FALSE,
    col.names = TRUE
  )

  # Filter year
  ResultDataTA <- ResultDataTA[ResultDataTA$YEAR == year, ]
  ResultDataTC <- ResultDataTC[ResultDataTC$YEAR == year, ]

  if (nrow(ResultDataTC) == 0) {
    write("Empty TC data frame for the selected year.", file = Errors, append = TRUE)
    return(TRUE)
  }

  write(
    paste("\n----------- Check consistency of maturity stages in",
          unique(ResultDataTC$TYPE_OF_FILE),
          "using spawning season info -",
          year),
    file = Errors,
    append = TRUE
  )

  # Add species and maturity codes
  ResultDataTC$Species <- paste(ResultDataTC$GENUS, ResultDataTC$SPECIES, sep = "")
  ResultDataTC$Maturity <- paste(
    as.character(ResultDataTC$MATURITY),
    ifelse(is.na(ResultDataTC$MATSUB), "", as.character(ResultDataTC$MATSUB)),
    sep = ""
  )

  # Remove rows with NA LENGTH_CLASS
  ResultDataTC <- ResultDataTC[!is.na(ResultDataTC$LENGTH_CLASS), ]

  maturity_table <- Maturity_parameters
  maturity_table$Species <- gsub(" ", "", maturity_table$Species)
  species_list <- DataTargetSpecies

  warnings_list <- list()

  # Loop over species
  for (i in unique(ResultDataTC$Species)) {

    ResultData_temp <- ResultDataTC[ResultDataTC$Species == i, ]

    cau_fau_temp <- species_list$FAUNISTIC_CATEGORY[
      paste0(
        substring(species_list$SPECIES, 1, 4),
        substring(species_list$SPECIES, 5, 7)
      ) == i
    ]

    for (j in 1:nrow(ResultData_temp)) {

      maturity_minitable <- maturity_table[
        as.character(maturity_table$Species) == i &
          as.character(maturity_table$SEX) == as.character(ResultData_temp$SEX[j]),
      ]

      month <- ResultDataTA[
        ResultDataTA$HAUL_NUMBER == ResultData_temp$HAUL_NUMBER[j],
        "MONTH"
      ]

      if (nrow(maturity_minitable) != 0 && length(month) > 0 && !is.na(month)) {

        Start <- maturity_minitable$Start_reproductive_season[1]
        End <- maturity_minitable$End_reproductive_season[1]

        # Check NA
        if (!is.na(Start) && !is.na(End)) {

          max_L50_value <- maturity_minitable$max_L50[1]

          if (!is.na(max_L50_value)) {

            threshold <- (max_L50_value + 0.2 * max_L50_value) * 10

            # Determine condition for spawning period
            is_in_spawning <- if (Start < End) {
              all(month >= Start) & all(month <= End)
            } else {
              all(month >= Start) | all(month <= End)
            }

            # Check for immature individuals in spawning period
            if (is_in_spawning) {
              if (
                (as.character(ResultData_temp$Maturity[j]) %in% c("0", "1", "2A")) &&
                as.numeric(ResultData_temp$LENGTH_CLASS[j]) > threshold
              ) {
                # Warning: immature during spawning and large
                warnings_list[[length(warnings_list) + 1]] <- data.frame(
                  GSA = ResultData_temp$AREA[j],
                  Year = year,
                  Haul = ResultData_temp$HAUL_NUMBER[j],
                  Species = ResultData_temp$Species[j],
                  Sex = ResultData_temp$SEX[j],
                  Length_Class_mm = ResultData_temp$LENGTH_CLASS[j],
                  Maturity_Stage = ResultData_temp$Maturity[j],
                  Start_Spawning_Month = Start,
                  End_Spawning_Month = End,
                  Month_of_Catch = month,
                  Threshold_mm = round(threshold, 2),
                  Warning_Type = paste(
                    "Immature individual during spawning period with length above threshold (", round(threshold, 2), " mm)."
                  ),
                  stringsAsFactors = FALSE
                )
              }
            }
          }
        }
      }
    }

    # Second check: mature individuals outside spawning period
    maturity_sp_table <- maturity_table[
      as.character(maturity_table$Species) == i,
    ]

    if (nrow(maturity_sp_table) != 0) {

      for (sex in 1:nrow(maturity_sp_table)) {

        sm_val <- maturity_sp_table$smallest_mature_individual_observed[sex]
        sm_val_num <- NA
        if (!is.na(sm_val) && sm_val != "n.a.") {
          sm_val_num <- as.numeric(as.character(sm_val)) * 10
        }

        # Select data for this sex
        if (as.character(maturity_sp_table$SEX[sex]) != "C") {
          ResultData_temp2 <- ResultData_temp[
            as.character(ResultData_temp$SEX) == as.character(maturity_sp_table$SEX[sex]),
          ]
        } else {
          ResultData_temp2 <- ResultData_temp
        }

        # Remove NA lengths
        ResultData_temp2 <- ResultData_temp2[!is.na(ResultData_temp2$LENGTH_CLASS), ]

        # Only mature stages
        mature_codes <- switch(
          as.character(cau_fau_temp),
          "Ao" = "3",
          "B"  = c("2D", "2"),
          "C"  = c("3A", "3"),
          "Ae" = c("3A", "3"),
          c()
        )

        if (length(mature_codes) > 0) {

          ResultData_temp2 <- ResultData_temp2[
            as.character(ResultData_temp2$Maturity) %in% mature_codes,
          ]
        }

        if (nrow(ResultData_temp2) > 0) {

          Start_temp <- maturity_sp_table$Start_reproductive_season[sex]
          End_temp <- maturity_sp_table$End_reproductive_season[sex]

          if (!is.na(Start_temp) && !is.na(End_temp)) {

            for (k in 1:nrow(ResultData_temp2)) {

              month_temp <- ResultDataTA[
                ResultDataTA$HAUL_NUMBER == ResultData_temp2$HAUL_NUMBER[k],
                "MONTH"
              ]

              is_outside_spawning <- if (Start_temp < End_temp) {
                any(month_temp < Start_temp) | any(month_temp > End_temp)
              } else {
                any(month_temp < Start_temp) & any(month_temp > End_temp)
              }

              if (length(month_temp) > 0 && is_outside_spawning) {

                len <- as.numeric(ResultData_temp2$LENGTH_CLASS[k])

                # Two types of warning:
                if (!is.na(sm_val_num) && len < (sm_val_num - 0.1 * sm_val_num)) {
                  # Mature specimen outside spawning AND below threshold
                  warnings_list[[length(warnings_list) + 1]] <- data.frame(
                    GSA = ResultData_temp2$AREA[k],
                    Year = year,
                    Haul = ResultData_temp2$HAUL_NUMBER[k],
                    Species = ResultData_temp2$Species[k],
                    Sex = ResultData_temp2$SEX[k],
                    Length_Class_mm = len,
                    Maturity_Stage = ResultData_temp2$Maturity[k],
                    Start_Spawning_Month = Start_temp,
                    End_Spawning_Month = End_temp,
                    Month_of_Catch = month_temp,
                    Threshold_mm = round(sm_val_num - 0.1 * sm_val_num, 2),
                    Warning_Type = paste(
                      "Mature individual outside spawning period smaller than smallest mature specimen (",
                      round(sm_val_num - 0.1 * sm_val_num, 2), " mm )."
                    ),
                    stringsAsFactors = FALSE
                  )

                } else {
                  # Mature specimen outside spawning but above threshold
                  warnings_list[[length(warnings_list) + 1]] <- data.frame(
                    GSA = ResultData_temp2$AREA[k],
                    Year = year,
                    Haul = ResultData_temp2$HAUL_NUMBER[k],
                    Species = ResultData_temp2$Species[k],
                    Sex = ResultData_temp2$SEX[k],
                    Length_Class_mm = len,
                    Maturity_Stage = ResultData_temp2$Maturity[k],
                    Start_Spawning_Month = Start_temp,
                    End_Spawning_Month = End_temp,
                    Month_of_Catch = month_temp,
                    Threshold_mm = ifelse(is.na(sm_val_num), NA, round(sm_val_num - 0.1 * sm_val_num, 2)),
                    Warning_Type = paste(
                      "Mature individual outside spawning period."
                    ),
                    stringsAsFactors = FALSE
                  )
                }
              }
            }
          }
        }
      }
    }
  }

  # Write CSV
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
      paste(nrow(warnings_df),
            "warnings written to CSV file:", basename(ErrorsCSV)),
      file = Errors,
      append = TRUE
    )
  } else {
    write(
      "No inconsistencies detected. All maturity stages consistent with spawning period info.",
      file = Errors,
      append = TRUE
    )
  }

  write(
    paste("Attention: if you change maturity stages after corrections, rerun the check because you might introduce duplicates in TC."),
    file = Errors,
    append = TRUE
  )

  return(TRUE)
}
