check_abundance <- function(ResultDataTA,
                           ResultDataTB,
                           year,
                           wd,
                           suffix = NULL,
                           distance_unit = "km",
                           min_hauls = 5L) #  "none", "month",
{



  if (FALSE) {
    TA <- ta# RoME::TA
    TB <- tb# RoME::TB
    wd <- "D:/OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L/RDB3/test"
    year=2005
    suffix = NULL
    distance_unit = "km"
    min_hauls = 5L

    plot_abundance(ta, tb, year = 2005, wd = wd)

  }


  # ---- run-time deps ---------------------------------------------------------
  if (!requireNamespace("dplyr", quietly = TRUE) ||
      !requireNamespace("ggplot2", quietly = TRUE) ||
      !requireNamespace("magrittr", quietly = TRUE))
    stop("Packages 'dplyr', 'ggplot2', 'magrittr' are required.")
  `%>%` <- magrittr::`%>%`

  km_per_unit <- if ((distance_unit) == "nmi") 1.852 else 1.0

  # --------------------------- logging setup ---------------------------------
  if (!dir.exists(file.path(wd, "Logfiles")))
    dir.create(file.path(wd, "Logfiles"), recursive = TRUE, showWarnings = FALSE)
  if (is.null(suffix))
    suffix <- paste(Sys.Date(), format(Sys.time(), "_time_h%Hm%Ms%OS0"), sep = "")
  log_file <- file.path(wd, "Logfiles", sprintf("Logfile_%s.dat", suffix))
  if (!file.exists(log_file)) file.create(log_file)
  numberError <- 0L
  write(sprintf("\n----------- Boxplot of Abundances - Year %s", year), file = log_file, append = TRUE)

  # ---- output directory -------------------------------------------------------
  out_dir <- file.path(wd, "Graphs", "Abundance")
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  # --------------------------- input filtering -------------------------------
  stopifnot(length(year) == 1L, is.numeric(year), !is.na(year))

  # ---- prep TA: swept area (km^2) -------------------------------------------
  TA <- ResultDataTA
  TB <- ResultDataTB
  swept <- TA %>%
    filter(YEAR == !!year) %>%
    mutate(across(c(DISTANCE, WING_OPENING), as.numeric),
           SW_AREA_KM2 = (DISTANCE * km_per_unit / 1000) * (WING_OPENING / 10000)) %>%
    select(COUNTRY, YEAR, MONTH, DAY, HAUL_NUMBER, SW_AREA_KM2)

  # ---- merge with TB & compute index ----------------------------------------
  idx <- TB %>%
    filter(YEAR == !!year) %>%
    select(COUNTRY, YEAR, MONTH, DAY, HAUL_NUMBER, GENUS, SPECIES,
           TOTAL_NUMBER_IN_THE_HAUL, FAUNISTIC_CATEGORY) %>%
    inner_join(swept, by = c("COUNTRY","YEAR","MONTH","DAY","HAUL_NUMBER"))

  # ---- data-quality checks ---------------------------------------------------
  # 1) missing swept area ------------------------------------------------------
  bad_sw <- idx %>% filter(is.na(SW_AREA_KM2) | SW_AREA_KM2 == 0)
  if (nrow(bad_sw) > 0) {
    for (ii in seq_len(nrow(bad_sw))) {
      bw <- bad_sw[ii, ]
      write(sprintf("Error: Haul %s %s %s: missing or zero swept area (removed from analysis).",
                    bw$HAUL_NUMBER, bw$GENUS, bw$SPECIES),
            file = log_file, append = TRUE)
      numberError <- numberError + 1
    }

    idx <- anti_join(idx, bad_sw, by = colnames(bad_sw))
  }
  allowed_zero <- c("V","G","H","D","E")
  bad_total <- idx %>%
    filter(is.na(TOTAL_NUMBER_IN_THE_HAUL) |
             (TOTAL_NUMBER_IN_THE_HAUL == 0 & !(FAUNISTIC_CATEGORY %in% allowed_zero)))
  if (nrow(bad_total) > 0) {
    for (ii in seq_len(nrow(bad_total))) {
      bt <- bad_total[ii, ]
      msg <- sprintf("Warning: Haul %s %s %s TOTAL_NUMBER_IN_THE_HAUL = %s (faunistic cat. %s) - removed from analysis.",
                     bt$HAUL_NUMBER, bt$GENUS, bt$SPECIES,
                     ifelse(is.na(bt$TOTAL_NUMBER_IN_THE_HAUL), "NA", bt$TOTAL_NUMBER_IN_THE_HAUL),
                     bt$FAUNISTIC_CATEGORY)
      write(msg, file = log_file, append = TRUE)
      # numberError <- numberError + 1
    }

    idx <- anti_join(idx, bad_total, by = colnames(bad_total))
  }


  if (nrow(idx) == 0) {
    write("Warning: no valid hauls left after QC filters. Plot not produced.",
          file = log_file, append = TRUE)
    return(TRUE)
  }

  idx <- idx %>%
    mutate(Abund = TOTAL_NUMBER_IN_THE_HAUL / SW_AREA_KM2,
           SPECIES_CODE = paste(GENUS, SPECIES, sep = ""))

  # ---- species filtering -----------------------------------------------------
  keep_sp <- idx %>%
    group_by(SPECIES_CODE) %>%
    summarise(n_hauls = n(), .groups = "drop") %>%
    filter(n_hauls >= min_hauls) %>%
    arrange(SPECIES_CODE) %>%
    pull(SPECIES_CODE)

  idx <- idx %>% filter(SPECIES_CODE %in% keep_sp) %>% arrange(SPECIES_CODE)
  if (nrow(idx) == 0) {
    write("Warning: no species meet the 'min_hauls' threshold. Plot not produced.",
          file = log_file, append = TRUE)
    return(invisible(NULL))
  }

  # ---- split species into chunks of 36 --------------------------------------
  species_order <- sort(unique(idx$SPECIES_CODE))
  chunks <- split(species_order, ceiling(seq_along(species_order) / 36))
  part_counter <- 1L
  saved_files  <- character(0)

  sp_chunk <- chunks[[1]]
  for (sp_chunk in chunks) {

    sub_idx <- idx %>% filter(SPECIES_CODE %in% sp_chunk)

    file_name <- file.path(out_dir, sprintf("Abundance_%d_plot%02d.jpeg", year, part_counter))



    g_sub <- ggplot(sub_idx, aes(x = "", y = Abund)) +
      geom_boxplot(outlier.colour = "blue", outlier.shape = 16) +
      facet_wrap(~ SPECIES_CODE, scales = "free_y") +
      labs(x = "", y = expression(Individuals~per~km^2),
           title = bquote(MEDITS ~ .(year) ~ "- Species abundance (plot" ~ .(part_counter) ~ ")")) +
      theme_bw()

    ggsave(file_name, g_sub, width = 9, height = 8, dpi = 150)

    saved_files <- c(saved_files, sprintf("Abundance_%d_plot%02d.jpeg", year, part_counter))
    write(sprintf("Saved plot: %s", sprintf("Abundance_%d_plot%02d.jpeg", year, part_counter)), file = log_file, append = TRUE)
    part_counter <- part_counter + 1L
  }


  # ---- final logging --------------------------------------------------------
  if (numberError == 0L) {
    write("No errors detected.", file = log_file, append = TRUE)
    return(TRUE)
  } else {
    # write(sprintf("QC completed with %d issue row(s) removed.", numberError),
    #       file = log_file, append = TRUE)
    return(FALSE)
  }
}



utils::globalVariables(c(
  "Abund",
  "COUNTRY",
  "DAY",
  "DISTANCE",
  "FAUNISTIC_CATEGORY",
  "GENUS",
  "HAUL_NUMBER",
  "MONTH",
  "SPECIES",
  "SPECIES_CODE",
  "SW_AREA_KM2",
  "TOTAL_NUMBER_IN_THE_HAUL",
  "WING_OPENING",
  "n_hauls"
))

utils::globalVariables(c(
  "anti_join", "n", "arrange", "pull"
))
