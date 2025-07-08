############################################################################################################################
#   RoME: R code to perform multiple checks on MEDITS Survey data (TA, TB, TC and TE files)                                #
#   Authors: I. Bitetto, W. Zupa, M.T. Spedicato                                                                           #
#   Coispa Tecnologia & Ricerca - Stazione sperimentale per lo Studio delle Risorse del Mare                               #
#   If you have any comments or suggestions please contact the following e-mail address: bitetto@coispa.it, zupa@coispa.it #
#   January 2022                                                                                                           #
############################################################################################################################


# Check if weights and numbers in TB are consistent

if (FALSE){
    wd <- "D:\\OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L\\RDB3\\test"
    ResultDataTB = RoME::TB # RoME::TB
    ResultDataTB <- read.table("D:\\OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L\\______ MEDITS DATA __OFFICIAL___\\MEDBSsurvey\\Demersal\\TB_MEDITS_FORMAT_2025.csv",sep=";",header=TRUE)
    ResultDataTB <- ResultDataTB[ResultDataTB$AREA==18, ]
    year=2015
    DataTargetSpecies <- RoME::DataTargetSpecies
    suffix="Check_Weight"
    check_weight(ResultDataTB, year, DataTargetSpecies, wd, suffix)
  }


############################################################################################################################
#   RoME: R code to perform multiple checks on MEDITS Survey data (TA, TB, TC and TE files)                                #
#   Authors: I. Bitetto, W. Zupa, M.T. Spedicato                                                                           #
#   Coispa Tecnologia & Ricerca - Stazione sperimentale per lo Studio delle Risorse del Mare                               #
#   If you have any comments or suggestions please contact the following e-mail address: bitetto@coispa.it, zupa@coispa.it #
#   Modified: July 2025                                                                                                    #
############################################################################################################################

if (FALSE){
  wd <- "D:\\OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L\\RDB3\\test"
  ResultDataTB <- read.table("D:\\OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L\\______ MEDITS DATA __OFFICIAL___\\MEDBSsurvey\\Demersal\\TB_MEDITS_FORMAT_2025.csv", sep=";", header=TRUE)
  ResultDataTB <- ResultDataTB[ResultDataTB$AREA == 18, ]
  year <- 2015
  DataTargetSpecies <- read.table("D:\\OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L\\RDB3\\data\\pesi_medi_per_specie.csv", sep=";", header=TRUE)
  suffix <- "Check_Weight"
  check_weight(ResultDataTB, year, DataTargetSpecies, wd, suffix)
}

check_weight <- function(ResultDataTB, year, DataTargetSpecies = DataTargetSpecies, wd, suffix) {

  oldpar <- par()
  on.exit(suppressWarnings(par(oldpar)))

  if (!file.exists(file.path(wd, "Logfiles"))) {
    dir.create(file.path(wd, "Logfiles"), recursive = TRUE, showWarnings = FALSE)
  }

  if (!exists("suffix")) {
    suffix <- paste(as.character(Sys.Date()), format(Sys.time(), "_time_h%Hm%Ms%OS0"), sep = "")
  }

  if (!file.exists(file.path(wd, "Graphs"))) {
    dir.create(file.path(wd, "Graphs"), showWarnings = FALSE)
  }

  ResultDataTB <- ResultDataTB[ResultDataTB$YEAR == year, ]
  AREA <- unique(ResultDataTB$AREA)[1]

  Errors <- file.path(wd,"Logfiles",paste("Logfile_", suffix ,".dat",sep=""))
  if (!file.exists(Errors)){
    file.create(Errors)
  }


  ErrorsCSV <- file.path(wd, "Logfiles", paste("Check_Mean_Weights_Logfile_GSA", AREA, "_Year", year, "_", suffix, ".csv", sep = ""))

  # prepare list to collect CSV rows
  warnings_list <- list()

  # header for CSV file
  csv_header <- data.frame(
    GSA = integer(),
    Year = integer(),
    Species = character(),
    Haul = integer(),
    Mean_Weight = numeric(),
    WMIN05 = numeric(),
    WMAX95 = numeric(),
    TYPE_OF_FILE = character(),
    stringsAsFactors = FALSE
  )

  # write header to CSV
  write.table(
    csv_header,
    file = ErrorsCSV,
    sep = ";",
    row.names = FALSE,
    col.names = TRUE
  )

  Result <- ResultDataTB

  write(paste("\n----------- check consistency of weight and number TB - ", year), file = Errors, append = TRUE)

  Weight <- DataTargetSpecies

  ResultData <- Result[order(Result$HAUL_NUMBER), c("TYPE_OF_FILE", "HAUL_NUMBER", "GENUS", "SPECIES", "TOTAL_WEIGHT_IN_THE_HAUL", "TOTAL_NUMBER_IN_THE_HAUL")]
  ResultData$species <- paste(ResultData$GENUS, ResultData$SPECIES, sep = "")

  ResultData <- ResultData[ResultData$TOTAL_NUMBER_IN_THE_HAUL != 0 & !is.na(ResultData$TOTAL_NUMBER_IN_THE_HAUL), ]
  ResultData$mean_weight <- round(ResultData$TOTAL_WEIGHT_IN_THE_HAUL / ResultData$TOTAL_NUMBER_IN_THE_HAUL, 3)

  if (nrow(ResultData) > 0) {

    present <- aggregate(Result$GENUS, by = list(Result$GENUS, Result$SPECIES), FUN = "length")
    colnames(present) <- c("GENUS", "SPECIES", "occurrence")
    present$species <- paste(present$GENUS, present$SPECIES, sep = "")
    present$present <- FALSE

    nb_graphs <- 0
    nb_graphs_to_be_printed <- 0

    species_list <- sort(unique(ResultData$species))

    for (spec in species_list) {

      # subset per specie
      species_data <- ResultData[ResultData$species == spec & !is.infinite(ResultData$mean_weight), ]
      FoundInTable <- Weight[Weight$SPECIES == spec, ]
      FoundInTable <- FoundInTable[!is.na(FoundInTable$WMIN05), ]

      # Check all hauls of this species
      if (nrow(FoundInTable) != 0) {
        for (j in 1:nrow(species_data)) {
          if (species_data$mean_weight[j] < FoundInTable$WMIN05[1] |
              species_data$mean_weight[j] > FoundInTable$WMAX95[1]) {

            # write(
            #   paste(
            #     "Warning: Haul", species_data$HAUL_NUMBER[j],
            #     species_data$species[j],
            #     ": mean weight =", species_data$mean_weight[j], "g out of boundaries (",
            #     round(FoundInTable$WMIN05[1], 2), "-", round(FoundInTable$WMAX95[1], 2),
            #     "g; 5th-95th percentile) in", species_data$TYPE_OF_FILE[j]
            #   ),
            #   file = Errors, append = TRUE
            # )

            # collect row for CSV
            warnings_list[[length(warnings_list) + 1]] <- data.frame(
              GSA = AREA,
              Year = year,
              Species = species_data$species[j],
              Haul = species_data$HAUL_NUMBER[j],
              Mean_Weight = species_data$mean_weight[j],
              WMIN05 = FoundInTable$WMIN05[1],
              WMAX95 = FoundInTable$WMAX95[1],
              TYPE_OF_FILE = species_data$TYPE_OF_FILE[j],
              stringsAsFactors = FALSE
            )
          }
        }
      }

      # plotting block (limit to species with ??? 5 observations)
      if (nrow(species_data) >= 5) {
        if (present[present$species == spec, ]$present == FALSE) {
          nb_graphs_to_be_printed <- nb_graphs_to_be_printed + 1
          present[present$species == spec, ]$present <- TRUE
        }
      }
    }

    # Write warnings to CSV file sorted
    if (length(warnings_list) > 0) {

      # Convert the list of warning rows into a single data frame
      warnings_df <- do.call(rbind, warnings_list)

      # Sort the CSV rows alphabetically by Species and then by Haul number
      warnings_df <- warnings_df[order(warnings_df$Species, warnings_df$Haul), ]

      # Write the data frame to CSV, appending the rows without writing the header again
      write.table(
        warnings_df,
        file = ErrorsCSV,
        sep = ";",
        row.names = FALSE,
        col.names = FALSE,
        append = TRUE
      )

      # Write a summary message into the .dat file indicating how many rows were written
      write(
        paste(
          nrow(warnings_df), "rows written to CSV file",
          basename(ErrorsCSV),
          "- species with mean weights outside reference ranges."
        ),
        file = Errors,
        append = TRUE
      )

    } else {

      # Write a message indicating no outliers were found
      write("No outliers detected. All mean weights within reference ranges.", file = Errors, append = TRUE)
    }

    # Create paginated JPEGs
    if (nb_graphs_to_be_printed > 0) {
      present_true <- present[present$present == TRUE, ]
      present_true <- present_true[order(present_true$species), ]

      x <- nb_graphs_to_be_printed
      m <- 6
      nb_sheets <- ceiling(x / m)

      for (i in 1:nb_sheets) {

        from_idx <- (6 * i - 5)
        to_idx <- min(6 * i, nrow(present_true))

        jpeg(filename = file.path(wd, "Graphs",
                                  paste("check_mean_weight_GSA", AREA, "_", year, "_", i, ".jpeg", sep = "")),
             width = 12, height = 8, units = "in", res = 150, quality = 75)

        par(
          mfrow = c(3, 2),
          mar = c(2.5, 2.5, 2, 1),
          oma = c(0.5, 0.5, 3.5, 0.5)
        )

        for (m in from_idx:to_idx) {
          species_name <- present_true$species[m]
          species_data <- ResultData[ResultData$species == species_name & !is.infinite(ResultData$mean_weight), ]
          xlabels <- species_data$HAUL_NUMBER
          X <- 1:length(xlabels)
          Y <- species_data$mean_weight

          if (length(X) != 0) {
            plot(X, Y,
                 main = paste(species_name, "-", year),
                 xlab = "HAUL number",
                 ylab = "mean weight",
                 type = "b",
                 pch = ".")
            text(X, Y, labels = xlabels)
          }
        }

        title_text <- paste("Check mean weight - GSA", AREA, "- Year", year, "- plot", i)
        mtext(
          title_text,
          outer = TRUE,
          side = 3,
          line = 1.2,
          cex = 1.4,
          font = 2
        )

        dev.off()
      }
    }

    if (nb_graphs > 0) {
      write("Warning: See graphs generated and saved in working directory about the species mean weight.", file = Errors, append = TRUE)
    }
  }

  write("No error occurred", file = Errors, append = TRUE)

  if (dev.cur() > 1) {
    while (dev.cur() > 1) {
      dev.off()
    }
  }

  ll <- list.files(tempdir())
  if (length(ll) > 0) {
    unlink(file.path(tempdir(), ll), recursive = TRUE)
  }

  return(TRUE)
}


###############################################################################

