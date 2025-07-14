check_swept_area <- function(ResultDataTA, year, wd, suffix) {

  if(FALSE){
    wd <- "D:\\OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L\\RDB3\\test"
    ResultDataTA <- read.table("D:/OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L/______ MEDITS DATA __OFFICIAL___/MEDBSsurvey/Demersal/TA_MEDITS_FORMAT_2025.csv",sep=";",header=TRUE)
    ResultDataTA <- ResultDataTA[ResultDataTA$AREA ==18, ]
    ResultDataTA[1,"DISTANCE"] <- NA
    year <- 2005
    suffix <- NA

    check_swept_area(ResultDataTA, year, wd, suffix)
  }

  ResultDataTA <- as.data.frame(ResultDataTA)

  numberError <- 0

  # Prepare folders
  if (!file.exists(file.path(wd, "Logfiles"))) {
    dir.create(file.path(wd, "Logfiles"), recursive = TRUE, showWarnings = FALSE)
  }
  if (!file.exists(file.path(wd, "Graphs"))) {
    dir.create(file.path(wd, "Graphs"), recursive = TRUE, showWarnings = FALSE)
  }

  if (is.null(suffix)) {
    suffix <- paste(as.character(Sys.Date()), format(Sys.time(), "_time_h%Hm%Ms%OS0"), sep = "")
  }

  Errors <- file.path(wd, "Logfiles", paste("Logfile_", suffix, ".dat", sep = ""))
  if (!file.exists(Errors)) {
    file.create(Errors)
  }

  # Check the year argument
  if (missing(year) || length(year) != 1 || is.na(year)) {
    stop("'year' argument should be a single numeric value")
  }

  write(paste("\n----------- check swept area - ", ResultDataTA$YEAR[1]), file = Errors, append = TRUE)

  # Filter dataset for the given year and valid hauls
  Dataset <- ResultDataTA[ResultDataTA$YEAR == year & ResultDataTA$VALIDITY == "V", ]

  if (nrow(Dataset) == 0) {
    write(paste("No valid hauls for year", year), file = Errors, append = TRUE)
    return(TRUE)
  }

  # Check explicitly for non-numeric values before numeric conversion
  cols_to_check <- c("WING_OPENING", "DISTANCE", "SHOOTING_DEPTH", "HAULING_DEPTH")

  for (col in cols_to_check) {
    invalid_rows <- which(!is.na(Dataset[[col]]) &
                            suppressWarnings(is.na(as.numeric(Dataset[[col]]))))

    if (length(invalid_rows) > 0) {
      for (i in invalid_rows) {
        write(
          paste("ERROR: Haul", Dataset$HAUL_NUMBER[i],
                ": non-numeric value '", Dataset[[col]][i],
                "' found in column", col, "in", Dataset$TYPE_OF_FILE[i]),
          file = Errors,
          append = TRUE
        )
        numberError <- numberError + 1
      }
    }
  }

  # Convert key columns to numeric
  for (col in cols_to_check) {
    Dataset[[col]] <- suppressWarnings(as.numeric(as.character(Dataset[[col]])))
  }

  # Check NA in WING_OPENING
  if (any(is.na(Dataset$WING_OPENING))) {
    na.results <- Dataset[is.na(Dataset$WING_OPENING), ]

    msg <- paste(
      "ERROR: Haul", na.results$HAUL_NUMBER,
      ": empty or NA value in 'WING_OPENING' in",
      na.results$TYPE_OF_FILE
    )

    write(msg, file = Errors, append = TRUE)

    numberError <- numberError + nrow(na.results)
  }

  # Check NA in DISTANCE
  if (any(is.na(Dataset$DISTANCE))) {
    na.results <- Dataset[is.na(Dataset$DISTANCE), ]

    msg <- paste(
      "ERROR: Haul", na.results$HAUL_NUMBER,
      ": empty or NA value in 'DISTANCE' in",
      na.results$TYPE_OF_FILE
    )

    write(msg, file = Errors, append = TRUE)

    numberError <- numberError + nrow(na.results)
  }

  # Calculate mean depth
  Dataset$mean_depth <- rowMeans(
    cbind(Dataset$SHOOTING_DEPTH, Dataset$HAULING_DEPTH),
    na.rm = TRUE
  )

  # Calculate swept area (in km?)
  Dataset$swept_area_km2 <- (Dataset$WING_OPENING / 10) * Dataset$DISTANCE / 1e6

  # Check rows where swept area is missing or negative
  invalid_rows <- Dataset[is.na(Dataset$swept_area_km2) | Dataset$swept_area_km2 < 0, ]
  if (nrow(invalid_rows) > 0) {
    for (i in seq_len(nrow(invalid_rows))) {
      write(
        paste("ERROR: Haul", invalid_rows$HAUL_NUMBER[i],
              ": swept area cannot be estimated or is negative.",
              "Computed value:", invalid_rows$swept_area_km2[i]),
        file = Errors,
        append = TRUE
      )
      numberError <- numberError + 1
    }
  }

  # Prepare data for plotting, removing NA rows
  plot_data <- Dataset[!is.na(Dataset$swept_area_km2) &
                         !is.na(Dataset$mean_depth),
                       c("HAUL_NUMBER", "mean_depth", "swept_area_km2")]

  # Plot anyway if there are data left
  if (nrow(plot_data) > 0) {
    jpeg_filename <- file.path(
      wd, "Graphs",
      paste0("swept_area_vs_depth_", year, ".jpeg")
    )


    max_depth <- min(max(plot_data$mean_depth, na.rm = TRUE), 1000)+ min(max(plot_data$mean_depth, na.rm = TRUE), 1000)*0.05

    # Crea un dataframe con le due bande rettangolari
    rect_data <- tibble(
      xmin = c(0, 200),
      xmax = c(200, max_depth),
      ymin = -Inf,
      ymax = Inf,
      fill_color = c("#e0f3f8", "#abd9e9")  # colori tenui a tua scelta
    )

    # Costruisci il plot
    p <- suppressWarnings(
      ggplot() +
        # layer dei rettangoli
        geom_rect(
          data = rect_data,
          aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill_color),
          alpha = 0.4,
          color = NA
        ) +
        # layer dei punti
        geom_point(
          data = plot_data,
          aes(x = mean_depth, y = swept_area_km2),
          color = "blue"
        ) +
        # label repulse
        geom_text_repel(
          data = plot_data,
          aes(x = mean_depth, y = swept_area_km2, label = HAUL_NUMBER),
          size = 3,
          max.overlaps = 20,
          box.padding = 0.05,
          point.padding = 0.002,
          segment.color = NA
        ) +
        scale_fill_identity() +  # usa i colori definiti così come sono
        labs(
          x = "Mean Depth (m)",
          y = bquote("Swept area (km"^2*")"),
          title = paste("Swept Area vs Depth - Year", year)
        ) +
        # coord_cartesian(xlim = c(0, 1000)) +
        theme_bw()
    )

    # print(p)

    # Save plot as JPEG
    suppressMessages(ggsave(
      filename = jpeg_filename,
      plot = p,
      width = 7,
      height = 4,
      units = "in",
      dpi = 150,
      device = "jpeg"
    )
    )
    write("Warning: Check the plot of swept area vs depth. It has been saved in the Graphs folder.", file = Errors, append = TRUE)
  } else {
    write("Plot not generated: no valid data after removing NAs.", file = Errors, append = TRUE)
  }

  # Final message
  if (numberError == 0) {
    write("No error occurred", file = Errors, append = TRUE)
    return(TRUE)
  } else {
    return(FALSE)
  }
}


utils::globalVariables(c(
  "fill_color", "HAUL_NUMBER", "mean_depth", "swept_area_km2", "xmax", "xmin", "ymax", "ymin"
))
