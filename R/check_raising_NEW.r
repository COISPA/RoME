############################################################################################################################
#   RoME: R code to perform multiple checks on MEDITS Survey data (TA, TB, TC and TE files)                                #
#   Authors: I. Bitetto, W. Zupa, M.T. Spedicato                                                                           #
#   Coispa Tecnologia & Ricerca - Stazione sperimentale per lo Studio delle Risorse del Mare                               #
#   If you have any comments or suggestions please contact the following e-mail address: bitetto@coispa.it, zupa@coispa.it #
#   January 2022                                                                                                           #
############################################################################################################################
# Check if, in case of sub-sampling in TC, the number per sex in TB is raised correctly


check_raising <- function(ResultDataTB, ResultDataTC, year, wd, suffix = NULL) {
  # suppressPackageStartupMessages(library(dplyr))

  # --------------------------- Logging setup ----------------------------------
  if (!dir.exists(file.path(wd, "Logfiles")))
    dir.create(file.path(wd, "Logfiles"), recursive = TRUE, showWarnings = FALSE)
  if (is.null(suffix))
    suffix <- paste(Sys.Date(), format(Sys.time(), "_time_h%Hm%Ms%OS0"), sep = "")
  log_file <- file.path(wd, "Logfiles", sprintf("Logfile_%s.dat", suffix))
  if (!file.exists(log_file)) file.create(log_file)
  numberError <- 0L
  write(sprintf("\n----------- CHECK RAISING - Year %s", year), file = log_file, append = TRUE)

  # --------------------------- Input filtering --------------------------------
  stopifnot(length(year) == 1L, is.numeric(year), !is.na(year))
  ResultDataTB <- ResultDataTB[ResultDataTB$YEAR == year, ]
  ResultDataTC <- ResultDataTC[ResultDataTC$YEAR == year, ]

  # --------------------------- 1. Weight consistency --------------------------
  tc_haul <- ResultDataTC %>%
    mutate(across(c(WEIGHT_OF_THE_FRACTION, WEIGHT_OF_THE_SAMPLE_MEASURED), as.numeric)) %>%
    group_by(COUNTRY, YEAR, HAUL_NUMBER, GENUS, SPECIES) %>%
    summarise(n_subsamples = n_distinct(WEIGHT_OF_THE_FRACTION),
              total_fraction = sum(unique(WEIGHT_OF_THE_FRACTION), na.rm = TRUE),
              .groups = "drop")

  mismatch_weight <- ResultDataTB %>%
    select(COUNTRY, YEAR, HAUL_NUMBER, GENUS, SPECIES, TOTAL_WEIGHT_IN_THE_HAUL) %>%
    inner_join(tc_haul, by = c("COUNTRY","YEAR","HAUL_NUMBER","GENUS","SPECIES")) %>%
    filter(n_subsamples > 1 & abs(total_fraction - TOTAL_WEIGHT_IN_THE_HAUL) > .Machine$double.eps)

  if (nrow(mismatch_weight) > 0) {
    for (k in seq_len(nrow(mismatch_weight))) {
      row <- mismatch_weight[k, ]
      msg <- sprintf("Haul %s %s %s TOTAL_WEIGHT_IN_HAUL in TB (%.3f) differs from sum of fractions (%.3f) in TC.",
                     row$HAUL_NUMBER, row$GENUS, row$SPECIES,
                     row$TOTAL_WEIGHT_IN_THE_HAUL, row$total_fraction)
      write(msg, file = log_file, append = TRUE)
    }
    numberError <- numberError + nrow(mismatch_weight)
  }

  # --------------------------- 2. Raising factor ------------------------------
  tc_expanded <- ResultDataTC %>%
    mutate(across(c(WEIGHT_OF_THE_FRACTION, WEIGHT_OF_THE_SAMPLE_MEASURED), as.numeric),
           codedsex = if_else(SEX %in% c("I", "N"), "I", as.character(SEX))) %>%
    group_by(COUNTRY, YEAR, HAUL_NUMBER, GENUS, SPECIES, codedsex,
             WEIGHT_OF_THE_FRACTION, WEIGHT_OF_THE_SAMPLE_MEASURED) %>%
    summarise(N = sum(NUMBER_OF_INDIVIDUALS_IN_THE_LENGTH_CLASS_AND_MATURITY_STAGE, na.rm = TRUE),
              .groups = "drop") %>%
    mutate(WEIGHT_OF_THE_SAMPLE_MEASURED = if_else(WEIGHT_OF_THE_SAMPLE_MEASURED == 0,
                                                   .Machine$double.eps,
                                                   WEIGHT_OF_THE_SAMPLE_MEASURED),
           molt   = WEIGHT_OF_THE_FRACTION / WEIGHT_OF_THE_SAMPLE_MEASURED,
           raised = N * molt)

  bad_ratio <- tc_expanded %>% filter(molt < 1 - 1e-10)
  if (nrow(bad_ratio) > 0) {
    for (k in seq_len(nrow(bad_ratio))) {
      row <- bad_ratio[k, ]
      msg <- sprintf("Haul %s %s %s %s %s raising factor %.3f < 1", row$HAUL_NUMBER,
                     row$COUNTRY, row$GENUS, row$SPECIES, row$codedsex, row$molt)
      write(msg, file = log_file, append = TRUE)
    }
    numberError <- numberError + nrow(bad_ratio)
  }

  # Totals raised per sex and overall
  tc_by_sex <- tc_expanded %>%
    group_by(COUNTRY, YEAR, HAUL_NUMBER, GENUS, SPECIES, SEX = codedsex) %>%
    summarise(RaisedSex = sum(raised, na.rm = TRUE), .groups = "drop")

  tc_total <- tc_by_sex %>%
    group_by(COUNTRY, YEAR, HAUL_NUMBER, GENUS, SPECIES) %>%
    summarise(RaisedTotal = sum(RaisedSex, na.rm = TRUE), .groups = "drop")

  # --------------------------- 3. Sex-specific comparison ----------------------
  required_nb <- c("NB_OF_FEMALES", "NB_OF_MALES", "NB_OF_UNDETERMINED")
  if (!all(required_nb %in% names(ResultDataTB)))
    stop("TB dataset lacks the required NB_ columns")

  tb_sex <- ResultDataTB %>%
    select(COUNTRY, YEAR, HAUL_NUMBER, GENUS, SPECIES, all_of(required_nb))

  for (k in seq_len(nrow(tc_by_sex))) {
    tc_row <- tc_by_sex[k, ]
    tb_row <- tb_sex %>% filter(COUNTRY == tc_row$COUNTRY, GENUS == tc_row$GENUS,
                                SPECIES == tc_row$SPECIES, HAUL_NUMBER == tc_row$HAUL_NUMBER)
    if (nrow(tb_row) == 0) {
      write(sprintf("Haul %s %s %s %s: missing row in TB - cannot compare by sex.",
                    tc_row$HAUL_NUMBER, tc_row$COUNTRY, tc_row$GENUS, tc_row$SPECIES),
            file = log_file, append = TRUE)
      numberError <- numberError + 1L
      next
    }
    lbl <- switch(tc_row$SEX, F = "FEMALES", M = "MALES", "UNDETERMINED")
    tb_val <- tb_row[[paste0("NB_OF_", lbl)]]
    if (!isTRUE(all.equal(round(tc_row$RaisedSex, 0), round(tb_val, 0)))) {
      msg <- sprintf("Haul %s %s %s %s NUMBER_OF_%s in TB (%d) not consistent with raised total (%d) in TC",
                     tc_row$HAUL_NUMBER, tc_row$COUNTRY, tc_row$GENUS, tc_row$SPECIES,
                     lbl, round(tb_val, 0), round(tc_row$RaisedSex, 0))
      write(msg, file = log_file, append = TRUE)
      numberError <- numberError + 1L
    }
  }

  # --------------------------- 4. Total individuals TC???TB ---------------------
  if (!"TOTAL_NUMBER_IN_THE_HAUL" %in% names(ResultDataTB))
    stop("TB dataset lacks TOTAL_NUMBER_IN_THE_HAUL column")

  tb_total <- ResultDataTB %>%
    select(COUNTRY, YEAR, HAUL_NUMBER, GENUS, SPECIES, TOTAL_NUMBER_IN_THE_HAUL)

  cmp_total <- inner_join(tc_total, tb_total, by = c("COUNTRY","YEAR","HAUL_NUMBER","GENUS","SPECIES"))

  for (k in seq_len(nrow(cmp_total))) {
    row <- cmp_total[k, ]
    if (!isTRUE(all.equal(round(row$RaisedTotal, 0), round(row$TOTAL_NUMBER_IN_THE_HAUL, 0)))) {
      msg <- sprintf("Haul %s %s %s %s TOTAL_NUMBER_IN_THE_HAUL in TB (%d) not consistent with raised total (%d) in TC",
                     row$HAUL_NUMBER, row$COUNTRY, row$GENUS, row$SPECIES,
                     round(row$TOTAL_NUMBER_IN_THE_HAUL, 0), round(row$RaisedTotal, 0))
      write(msg, file = log_file, append = TRUE)
      numberError <- numberError + 1L
    }
  }

  # --------------------------- 5. Internal TB consistency ---------------------
  tb_internal <- ResultDataTB %>%
    mutate(SumSexTB = rowSums(across(all_of(required_nb)), na.rm = TRUE))

  for (k in seq_len(nrow(tb_internal))) {
    row <- tb_internal[k, ]
    if (!isTRUE(all.equal(round(row$SumSexTB, 0), round(row$TOTAL_NUMBER_IN_THE_HAUL, 0)))) {
      msg <- sprintf("Haul %s %s %s %s TOTAL_NUMBER_IN_THE_HAUL in TB (%d) not equal to the sum of sexes (%d) in TB",
                     row$HAUL_NUMBER, row$COUNTRY, row$GENUS, row$SPECIES,
                     round(row$TOTAL_NUMBER_IN_THE_HAUL, 0), round(row$SumSexTB, 0))
      write(msg, file = log_file, append = TRUE)
      numberError <- numberError + 1L
    }
  }

  # --------------------------- Return value -----------------------------------
  if (numberError == 0L) {
    write("No error occurred", file = log_file, append = TRUE)
    return(TRUE)
  } else {
    return(FALSE)
  }
}




utils::globalVariables(
  c(
    # Column names from TB / TC tables
    "COUNTRY", "YEAR", "HAUL_NUMBER", "GENUS", "SPECIES",
    "WEIGHT_OF_THE_FRACTION", "WEIGHT_OF_THE_SAMPLE_MEASURED",
    "NUMBER_OF_INDIVIDUALS_IN_THE_LENGTH_CLASS_AND_MATURITY_STAGE",
    "SEX", "NB_OF_FEMALES", "NB_OF_MALES", "NB_OF_UNDETERMINED",
    "TOTAL_WEIGHT_IN_THE_HAUL", "TOTAL_NUMBER_IN_THE_HAUL",

    # Temporary variables created inside dplyr pipelines
    "codedsex", "N", "molt", "raised",
    "RaisedSex", "RaisedTotal",
    "n_subsamples", "total_fraction",
    "SumSexTB"
  )
)
