############################################################################################################################
#   RoME: R code to perform multiple checks on MEDITS Survey data (TA, TB, TC and TE files)                                #
#   Authors: I. Bitetto, W. Zupa, M.T. Spedicato                                                                           #
#   Coispa Tecnologia & Ricerca - Stazione sperimentale per lo Studio delle Risorse del Mare                               #
#   If you have any comments or suggestions please contact the following e-mail address: bitetto@coispa.it, zupa@coispa.it #
#   January 2022                                                                                                           #
############################################################################################################################
# Check if weight in TE are consistent with length-weight relationship
if (FALSE) {
  # library(RoME)
  wd <- tempdir()
  suffix <- NA # paste(as.character(Sys.Date()),format(Sys.time(), "_time_h%Hm%Ms%OS0"),sep="")
  year <- 2024

  DataTE <- te # RoME::TE # read.csv("~/GitHub/RoME/data/TE_2012-2018 _GSA18.csv", sep=";")
  SPECIES <- NA
  SEX <- NA
  AREA <- NA
  a <- NA
  b <- NA
  verbose <- FALSE
  check_individual_weightTE(DataTE = TE, LW = ab_parameters, year, wd = wd, suffix = suffix, verbose = TRUE)
}

check_individual_weightTE <- function(DataTE, LW = NA, year, wd, suffix, verbose = FALSE) {
  oldpar <- par(no.readonly = TRUE)
  on.exit(suppressWarnings(par(oldpar$mfraw)))

  if (!file.exists(file.path(wd, "Logfiles"))) {
    dir.create(file.path(wd, "Logfiles"), recursive = TRUE, showWarnings = FALSE)
  }
  if (!file.exists(file.path(wd, "Graphs"))) {
    dir.create(file.path(wd, "Graphs"), showWarnings = FALSE)
  }
  numberError <- 0
  if (!exists("suffix")) {
    suffix <- paste(as.character(Sys.Date()), format(Sys.time(), "_time_h%Hm%Ms%OS0"), sep = "")
  }
  Errors <- file.path(wd, "Logfiles", paste("Logfile_", suffix, ".dat", sep = ""))
  if (!file.exists(Errors)) {
    file.create(Errors)
  }

  ### FILTERING DATA FOR THE SELECTED YEAR
  arg <- "year"
  if (!exists(arg)) {
    stop(paste0("'", arg, "' argument should be provided"))
  } else if (length(year) != 1) {
    stop(paste0("only one value should be provided for '", arg, "' argument"))
  } else if (is.na(year)) {
    stop(paste0(arg, " argument should be a numeric value"))
  }
  DataTE <- DataTE[DataTE$YEAR == year, ]
  ########################################

  TE <- DataTE[!is.na(DataTE$AREA),]
  TE <- TE[!is.na(TE$LENGTH_CLASS), ]

  write(paste("\n----------- check consistency individual weights in TE - ", TE$YEAR[1]), file = Errors, append = TRUE)
  numberError <- 0
  numberError_ <- 0

  TE_ND <- TE[as.character(TE$INDIVIDUAL_WEIGHT) == "ND", ] # selection on the weight ND
  TE_ND$Species <- paste(TE_ND$GENUS, TE_ND$SPECIES)
  species <- unique(TE_ND$Species)


  list_g1_g2 <- RoME::list_g1_g2
  G1 <- data.frame(as.character(list_g1_g2[!is.na(list_g1_g2$MEDITS_G1), "CODE"]))

  for (spe in species) {
    if ((spe %in% G1[, 1])) {
      write(paste("For G1 species the individual weight in TE is mandatory. Please check,", spe), file = Errors, append = TRUE)
      numberError_ <- numberError_ + 1
    }
  }

  TE <- TE[as.character(TE$INDIVIDUAL_WEIGHT) != "ND", ] # selection on the weight different from ND

  if (!inherits(LW, "data.frame")) {
    if (all(is.na(LW))) {
      if (verbose) {
        message("a and b parameters extracted from RoME LW table")
      }
      LW <- RoME::LW
    }
  }
  TE$mean_length <- NA
  TE$mean_weight <- NA
  TE$perc_diff <- NA

  species_to_plot <- as.character(unique(LW[, "SPECIES"])) # LW$AREA == TE$AREA[1]

  i <- 1
  for (i in 1:nrow(TE)) {

    ab <- LW[(LW$SPECIES == paste(TE$GENUS[i], TE$SPECIES[i])) & (as.character(LW$SEX) == as.character(TE$SEX[i])) & (LW$AREA==TE$AREA[1]), ] #

    if (nrow(ab)==0) {
      ab <- LW[(LW$SPECIES == paste(TE$GENUS[i], TE$SPECIES[i])) & (as.character(LW$SEX) == as.character(TE$SEX[i]))  , ] #
    }


    if (nrow(ab) != 0) {
      A <- ab$a[1]
      B <- ab$b[1]
      if (as.character(TE$LENGTH_CLASSES_CODE[i]) == "m") {
        mean_length <- TE$LENGTH_CLASS[i] + 0.5
      } else if (as.character(TE$LENGTH_CLASSES_CODE[i]) == "0") { # step: 0.5 cm
        mean_length <- (TE$LENGTH_CLASS[i] + 2.5) / 10
      } else if (as.character(TE$LENGTH_CLASSES_CODE[i]) == "1") { # step: 1 cm
        mean_length <- (TE$LENGTH_CLASS[i] + 5) / 10
      }
      mean_weight <- A * mean_length^B

      TE$mean_length[i] <- mean_length
      TE$mean_weight[i] <- mean_weight # estimated weight
      TE$perc_diff[i] <- (as.numeric(as.character(TE$INDIVIDUAL_WEIGHT[i])) - TE$mean_weight[i]) / as.numeric(as.character(TE$mean_weight[i])) * 100

      if (abs(TE$perc_diff[i]) > 20) {
        numberError <- numberError + 1
      }
    }
  }

  # scatter plots
  # ii=26
  for (ii in 1:length(species_to_plot)) {
    TE_temp1 <- TE[paste(TE$GENUS, TE$SPECIES) == species_to_plot[ii], ]
    if (nrow(TE_temp1) != 0) {
      # tiff(filename = file.path(wd, "Graphs", paste("check_individual_weight_", species_to_plot[ii], "_", TE$YEAR[1], ".tif", sep = "")), width = 8, height = 8, bg = "white", units = "in", res = 300, compression = "lzw", pointsize = 1 / 300)
	  jpeg(filename = file.path(wd, "Graphs", paste("check_individual_weight_", species_to_plot[ii], "_", TE$YEAR[1], ".jpeg", sep = "")), width = 8, height = 8, bg = "white", units = "in", res = 200, quality=80)
      par(mfrow = c(3, 1))
    }

    sex="M"
    for (sex in c("M", "F", "I", "N")) {
      TE_temp <- TE[paste(TE$GENUS, TE$SPECIES) == species_to_plot[ii] & as.character(TE$SEX) == sex, ]
      if (nrow(TE_temp) != 0) {

          ab <- LW[(LW$SPECIES == paste(TE$GENUS[i], TE$SPECIES[i])) & (as.character(LW$SEX) == as.character(TE$SEX[i])) & (LW$AREA==TE$AREA[1]), ]
        if (nrow(ab)==0) {
          ab <- LW[(LW$SPECIES == paste(TE$GENUS[i], TE$SPECIES[i])) & (as.character(LW$SEX) == as.character(TE$SEX[i]))  , ] #
        }

        if (nrow(ab) != 0) {
          A <- ab$a[1]
          B <- ab$b[1]
        }
        xx <- as.numeric(as.character(unique(TE_temp$mean_length[order(as.numeric(as.character(TE_temp$mean_length)))])))
        yy <- A * xx^B
        if (as.character(TE_temp$LENGTH_CLASSES_CODE[1]) == "m") {
          plot(as.numeric(as.character(TE_temp$mean_length)), as.numeric(as.character(TE_temp$INDIVIDUAL_WEIGHT)), main = paste("Length-weight relationship ", paste(TE_temp$GENUS[1], TE_temp$SPECIES[1]), " ", sex, " - ", TE_temp$YEAR[1]), xlab = "length (mm)", ylab = "weight (g)")
        } else {
          plot(as.numeric(as.character(TE_temp$mean_length)), as.numeric(as.character(TE_temp$INDIVIDUAL_WEIGHT)), main = paste("Length-weight relationship ", paste(TE_temp$GENUS[1], TE_temp$SPECIES[1]), " ", sex, " - ", TE_temp$YEAR[1]), xlab = "length (cm)", ylab = "weight (g)")
        }
        lines(xx, yy, col = "blue")
      }
    } # fine sex
    if (nrow(TE_temp1) != 0) {
      dev.off()
    }
  } # fine ciclo for

  if (numberError == 0) {
    write(paste("No error occurred"), file = Errors, append = TRUE)
  } else {
    write.table(TE, file = file.path(wd, paste("TE_with_estimated_weights_", TE$YEAR[1], ".csv", sep = "")), sep = ";", row.names = F)

    write("For some records the difference between estimated and observed individual weight is greater than 20%. Please verify in the file TE_with_estimated_weights.csv automatically produced in the working directory", file = Errors, append = TRUE)
  }

  # if (file.exists(file.path(tempdir(), "Logfiles"))) {
  #   unlink(file.path(tempdir(), "Logfiles"), recursive = T)
  # }
  # if (file.exists(file.path(tempdir(), "Graphs"))) {
  #   unlink(file.path(tempdir(), "Graphs"), recursive = T)
  # }
  # if (file.exists(file.path(tempdir(), "files R-Sufi"))) {
  #   unlink(file.path(tempdir(), "files R-Sufi"), recursive = T)
  # }
  #
  # unlink(file.path(tempdir(), list.files(file.path(tempdir()))), recursive = T)

  if (numberError_ == 0) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}
