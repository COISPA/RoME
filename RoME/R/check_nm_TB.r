############################################################################################################################
#   RoME: R code to perform multiple checks on MEDITS Survey data (TA, TB, TC and TE files)                                #
#   Authors: I. Bitetto, W. Zupa, M.T. Spedicato                                                                           #
#   Coispa Tecnologia & Ricerca - Stazione sperimentale per lo Studio delle Risorse del Mare                               #
#   If you have any comments or suggestions please contact the following e-mail address: bitetto@coispa.it, zupa@coispa.it #
#   January 2022                                                                                                           #
############################################################################################################################
# Check if in TB there are the total number, number of females, males and undetermined for species G1

check_nm_TB <- function(DataTB, year, wd, suffix) {
  if (FALSE) {
    wd <- tempdir()
    suffix <- paste(as.character(Sys.Date()), format(Sys.time(), "_time_h%Hm%Ms%OS0"), sep = "")
    DataTB <- tb # RoME::TB
    # DataTC = RoME::TC
    year <- 2015
    # check_nm_TB(DataTB,DataTC, year=2007, wd, suffix)
  }
  if (!file.exists(file.path(wd, "Logfiles"))) {
    dir.create(file.path(wd, "Logfiles"), recursive = TRUE, showWarnings = FALSE)
  }
  if (!file.exists(file.path(wd, "Graphs"))) {
    dir.create(file.path(wd, "Graphs"), showWarnings = FALSE)
  }
  if (!exists("suffix")) {
    suffix <- paste(as.character(Sys.Date()), format(Sys.time(), "_time_h%Hm%Ms%OS0"), sep = "")
  }
  numberError <- 0
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
  DataTB <- DataTB[DataTB$YEAR == year, ]
  ########################################

  TB <- DataTB

  write(paste("\n----------- check presence of number of individuals for species G1 - ", TB$YEAR[1]), file = Errors, append = TRUE)

  if (year >= 2012) {

    # SELECTION OF SPECIES G1
    TB <- TB[(as.character(TB$FAUNISTIC_CATEGORY) == "Ae") |
      (as.character(TB$GENUS) == "MERL" & as.character(TB$SPECIES) == "MERL") |
      (as.character(TB$GENUS) == "MULL") |
      (as.character(TB$GENUS) == "ARIS" & as.character(TB$SPECIES) == "FOL") |
      (as.character(TB$GENUS) == "ARIT" & as.character(TB$SPECIES) == "ANT") |
      (as.character(TB$GENUS) == "ILLE" & as.character(TB$SPECIES) == "COI") |
      (as.character(TB$GENUS) == "LOLI" & as.character(TB$SPECIES) == "VUL") |
      (as.character(TB$GENUS) == "NEPR" & as.character(TB$SPECIES) == "NOR") |
      (as.character(TB$GENUS) == "PAPE" & as.character(TB$SPECIES) == "LON"), ]

    if (nrow(TB) > 0) {
      TB$num_sum <- rowSums(TB[, c("TOTAL_NUMBER_IN_THE_HAUL", "NB_OF_FEMALES", "NB_OF_MALES", "NB_OF_UNDETERMINED")],na.rm=TRUE)

      i <- 1
      for (i in 1:nrow(TB)) {
        if (TB$num_sum[i]==0) {

            write(paste("Haul", TB$HAUL_NUMBER[i], TB$GENUS[i], TB$SPECIES[i], "for the fields related to numbers of individuals, the value 0 is not allowed from 2012 for species G1"), file = Errors, append = TRUE)
            numberError <- numberError + 1
        }
      }
    } # nrow>0
  } else {
    write(paste("The check is not performed for data collected before 2012"), file = Errors, append = TRUE)
    numberError <- 0
  } # year >=2012
  if (numberError == 0) {
    write(paste("No error occurred"), file = Errors, append = TRUE)
  }

  if (file.exists(file.path(tempdir(), "Logfiles"))) {
    unlink(file.path(tempdir(), "Logfiles"), recursive = TRUE)
  }
  if (file.exists(file.path(tempdir(), "Graphs"))) {
    unlink(file.path(tempdir(), "Graphs"), recursive = TRUE)
  }
  if (file.exists(file.path(tempdir(), "files R-Sufi"))) {
    unlink(file.path(tempdir(), "files R-Sufi"), recursive = TRUE)
  }
  if (numberError == 0) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}
