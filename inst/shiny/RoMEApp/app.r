library(shiny)
library(shinyjs)
# library(shinyFiles)
library(svDialogs)
library(RoME)

invisible(options(shiny.maxRequestSize = 200 * 1024^2))

# Check that uploaded table has exactly the expected column names
check_medits_colnames <- function(dat, tab_name) {
  expected_names <- as.character(
    RoME::classes$MEDITS[RoME::classes$table == tab_name]
  )

  actual_names <- colnames(dat)

  if (length(actual_names) != length(expected_names)) {
    stop(
      paste0(
        tab_name, ": wrong number of columns. Expected ",
        length(expected_names), ", found ", length(actual_names), "."
      )
    )
  }

  if (!identical(actual_names, expected_names)) {
    wrong_pos <- which(actual_names != expected_names)

    stop(
      paste0(
        tab_name, ": column names do not match the expected template.\n",
        paste(
          utils::head(
            paste0(
              "[", wrong_pos, "] expected='", expected_names[wrong_pos],
              "' found='", actual_names[wrong_pos], "'"
            ),
            15
          ),
          collapse = "\n"
        )
      )
    )
  }

  invisible(TRUE)
}

# Read MEDITS table without renaming columns
read_medits_file <- function(fileinfo, sep, tab_name) {
  req(fileinfo)

  dat <- read.table(
    fileinfo$datapath,
    sep = sep,
    header = TRUE,
    stringsAsFactors = FALSE,
    check.names = FALSE,
    na.strings = c("NA", "")
  )

  check_medits_colnames(dat, tab_name)

  dat
}

# Read optional reference object
read_reference_object <- function(fileinfo, sep = ";") {
  req(fileinfo)

  ext <- tolower(tools::file_ext(fileinfo$name))

  if (ext == "rds") {
    readRDS(fileinfo$datapath)
  } else {
    read.table(
      fileinfo$datapath,
      sep = sep,
      header = TRUE,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  }
}

# Apply the same targeted coercions used in the working manual script
apply_manual_rome_preprocessing <- function(ta, tb, tc, te = NA, tl = NA) {
  # TA coercions
  if ("SHOOTING_TIME" %in% names(ta)) ta$SHOOTING_TIME <- as.integer(ta$SHOOTING_TIME)
  if ("SHOOTING_DEPTH" %in% names(ta)) ta$SHOOTING_DEPTH <- as.integer(ta$SHOOTING_DEPTH)
  if ("HAULING_TIME" %in% names(ta)) ta$HAULING_TIME <- as.integer(ta$HAULING_TIME)
  if ("HAULING_DEPTH" %in% names(ta)) ta$HAULING_DEPTH <- as.integer(ta$HAULING_DEPTH)
  if ("DISTANCE" %in% names(ta)) ta$DISTANCE <- as.integer(ta$DISTANCE)
  if ("VERTICAL_OPENING" %in% names(ta)) ta$VERTICAL_OPENING <- as.integer(ta$VERTICAL_OPENING)
  if ("WING_OPENING" %in% names(ta)) ta$WING_OPENING <- as.integer(ta$WING_OPENING)
  if ("WARP_LENGTH" %in% names(ta)) ta$WARP_LENGTH <- as.integer(ta$WARP_LENGTH)
  if ("OBSERVATIONS" %in% names(ta)) ta$OBSERVATIONS <- as.integer(ta$OBSERVATIONS)
  if ("BOTTOM_SALINITY_BEGINNING" %in% names(ta)) ta$BOTTOM_SALINITY_BEGINNING <- as.numeric(ta$BOTTOM_SALINITY_BEGINNING)
  if ("BOTTOM_SALINITY_END" %in% names(ta)) ta$BOTTOM_SALINITY_END <- as.numeric(ta$BOTTOM_SALINITY_END)
  if ("MEASURING_SYSTEM_SALINITY" %in% names(ta)) ta$MEASURING_SYSTEM_SALINITY <- as.numeric(ta$MEASURING_SYSTEM_SALINITY)

  # TB coercions
  if ("TOTAL_WEIGHT_IN_THE_HAUL" %in% names(tb)) tb$TOTAL_WEIGHT_IN_THE_HAUL <- as.integer(tb$TOTAL_WEIGHT_IN_THE_HAUL)

  # TC coercions
  if ("WEIGHT_OF_THE_FRACTION" %in% names(tc)) tc$WEIGHT_OF_THE_FRACTION <- as.integer(tc$WEIGHT_OF_THE_FRACTION)
  if ("WEIGHT_OF_THE_SAMPLE_MEASURED" %in% names(tc)) tc$WEIGHT_OF_THE_SAMPLE_MEASURED <- as.integer(tc$WEIGHT_OF_THE_SAMPLE_MEASURED)
  if ("LENGTH_CLASS" %in% names(tc)) tc$LENGTH_CLASS <- as.integer(tc$LENGTH_CLASS)
  if ("NUMBER_OF_INDIVIDUALS_IN_THE_LENGTH_CLASS_AND_MATURITY_STAGE" %in% names(tc)) {
    tc$NUMBER_OF_INDIVIDUALS_IN_THE_LENGTH_CLASS_AND_MATURITY_STAGE <-
      as.integer(tc$NUMBER_OF_INDIVIDUALS_IN_THE_LENGTH_CLASS_AND_MATURITY_STAGE)
  }

  list(
    ta = ta,
    tb = tb,
    tc = tc,
    te = te,
    tl = tl
  )
}

# Return the min/max YEAR found in TA
get_year_range_from_ta <- function(ta) {
  if (is.null(ta) || !"YEAR" %in% names(ta)) {
    return(NULL)
  }

  years <- suppressWarnings(as.integer(as.character(ta$YEAR)))
  years <- years[!is.na(years)]

  if (length(years) == 0) {
    return(NULL)
  }

  c(min(years), max(years))
}

# Filter one table by YEAR if the column exists
filter_table_by_year <- function(dat, year_from, year_to) {
  if (length(dat) == 1 && is.atomic(dat) && is.na(dat)) {
    return(dat)
  }

  if (is.null(dat)) {
    return(dat)
  }

  if (!is.data.frame(dat)) {
    return(dat)
  }

  if (!"YEAR" %in% names(dat)) {
    return(dat)
  }

  years <- suppressWarnings(as.integer(as.character(dat$YEAR)))
  keep <- !is.na(years) & years >= year_from & years <= year_to

  dat[keep, , drop = FALSE]
}

ui <- fluidPage(
  useShinyjs(),

  tags$head(
    tags$style(HTML("
      body {
        background-color: #f7f7f7;
      }

      .app-header {
        text-align: center;
        padding: 15px 0 10px 0;
      }

      .app-subtitle {
        color: #444444;
        margin-top: -5px;
        margin-bottom: 20px;
      }

      .section-box {
        background-color: white;
        border: 1px solid #dddddd;
        border-radius: 10px;
        padding: 20px;
        margin-bottom: 20px;
        box-shadow: 0 1px 4px rgba(0, 0, 0, 0.05);
      }

      .section-title {
        font-size: 22px;
        font-weight: 600;
        margin-bottom: 15px;
        color: #1f2d3d;
      }

      .btn-main {
        min-width: 180px;
        font-size: 16px;
        font-weight: 600;
      }

      .log-box pre {
        background-color: #fafafa;
        border: 1px solid #d9d9d9;
        border-radius: 6px;
        padding: 12px;
        white-space: pre-wrap;
        word-wrap: break-word;
        max-height: 500px;
        overflow-y: auto;
      }

      .status-box pre {
        background-color: #fafafa;
        border: 1px solid #d9d9d9;
        border-radius: 6px;
        padding: 12px;
        white-space: pre-wrap;
      }
    "))
  ),

  div(
    class = "app-header",
    tags$img(
      src = "RoME_logo.png",
      height = "110px",
      alt = "RoME logo",
      style = "display: block; margin: 0 auto 12px auto;"
    ),
    # h1("RoME"),
    p("Consistency checks for MEDITS / Black Sea survey data", class = "app-subtitle")
  ),

  fluidRow(
    column(
      width = 12,
      div(
        class = "section-box",
        div(class = "section-title", "Input files"),
        fluidRow(
          column(
            width = 2,
            selectInput(
              "rome_fun",
              "Function",
              choices = c("RoMEcc", "RoMEBScc"),
              selected = "RoMEcc"
            )
          ),
          column(
            width = 2,
            fileInput("ta_file", "Upload TA file", accept = c(".csv", ".txt"))
          ),
          column(
            width = 2,
            fileInput("tb_file", "Upload TB file", accept = c(".csv", ".txt"))
          ),
          column(
            width = 2,
            fileInput("tc_file", "Upload TC file", accept = c(".csv", ".txt"))
          ),
          column(
            width = 2,
            fileInput("te_file", "Upload TE file (optional)", accept = c(".csv", ".txt"))
          ),
          column(
            width = 2,
            fileInput("tl_file", "Upload TL file (optional)", accept = c(".csv", ".txt"))
          )
        ),
        br(),
        fluidRow(
          column(
            width = 3,
            selectInput(
              "sep",
              "Main file separator",
              choices = c(";" = ";", "," = ","),
              selected = ";"
            )
          ),
          column(
            width = 3,
            textInput("suffix", "Suffix (optional)", value = "")
          ),
          column(
            width = 3,
            checkboxInput("zip", "Create zip output", value = TRUE)
          ),
          column(
            width = 3,
            checkboxInput("verbose", "Verbose", value = TRUE)
          )
        ),
        br(),
        fluidRow(
          column(
            width = 12,
            tags$label("Results directory"),
            div(
              style = "display: flex; gap: 10px; align-items: center;",
              actionButton(
                "browse_dir",
                "Browse...",
                style = "white-space: nowrap;"
              ),
              textInput(
                "wd_run",
                label = NULL,
                value = "D:/OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L/RDB3/test_shiny_RoME",
                width = "100%"
              )
            )
          )
        )
      )
    )
  ),

  fluidRow(
    column(
      width = 12,
      div(
        class = "section-box",
        div(class = "section-title", "Year selection"),
        fluidRow(
          column(
            width = 4,
            numericInput(
              "year_from",
              "First year to include",
              value = NA,
              min = NA,
              max = NA,
              step = 1
            )
          ),
          column(
            width = 4,
            numericInput(
              "year_to",
              "Last year to include",
              value = NA,
              min = NA,
              max = NA,
              step = 1
            )
          ),
          column(
            width = 4,
            div(
              style = "padding-top: 25px;",
              textOutput("year_info")
            )
          )
        )
      )
    )
  ),

  fluidRow(
    column(
      width = 12,
      div(
        class = "section-box",
        div(class = "section-title", "Optional custom reference tables"),
        fluidRow(
          column(
            width = 3,
            fileInput("stratification_file", "Custom Stratification", accept = c(".csv", ".txt", ".rds"))
          ),
          column(
            width = 3,
            fileInput("ref_list_file", "Custom Ref_list", accept = c(".csv", ".txt", ".rds"))
          ),
          column(
            width = 3,
            fileInput("target_species_file", "Custom DataTargetSpecies", accept = c(".csv", ".txt", ".rds"))
          ),
          column(
            width = 3,
            selectInput(
              "aux_sep",
              "Custom table separator",
              choices = c(";" = ";", "," = ","),
              selected = ";"
            )
          )
        ),
        br(),
        fluidRow(
          column(
            width = 3,
            fileInput("maturity_file", "Custom Maturity", accept = c(".csv", ".txt", ".rds"))
          ),
          column(
            width = 3,
            fileInput("ab_parameters_file", "Custom ab_parameters", accept = c(".csv", ".txt", ".rds"))
          ),
          column(
            width = 3,
            fileInput("stages_list_file", "Custom stages_list", accept = c(".csv", ".txt", ".rds"))
          ),
          column(
            width = 3,
            fileInput("assTL_file", "Custom assTL", accept = c(".csv", ".txt", ".rds"))
          )
        )
      )
    )
  ),

  fluidRow(
    column(
      width = 12,
      div(
        class = "section-box",
        div(class = "section-title", "Run settings"),
        fluidRow(
          column(
            width = 12,
            style = "text-align: center; margin-top: 10px;",
            actionButton("run_btn", "Run RoME checks", class = "btn-primary btn-main"),
            tags$span(style = "display:inline-block; width:15px;"),
            downloadButton("download_results", "Download results zip", class = "btn-default btn-main")
          )
        )
      )
    )
  ),

  fluidRow(
    column(
      width = 12,
      div(
        class = "section-box status-box",
        div(class = "section-title", "Status"),
        verbatimTextOutput("status")
      )
    )
  ),

  fluidRow(
    column(
      width = 12,
      div(
        class = "section-box log-box",
        div(class = "section-title", "Run log"),
        verbatimTextOutput("run_log")
      )
    )
  )
)

server <- function(input, output, session) {

  rv <- reactiveValues(
    status = "Waiting for input.",
    log = "",
    zipfile = NULL,
    run_dir = NULL,
    running = FALSE,
    ta = NULL,
    tb = NULL,
    tc = NULL,
    te = NA,
    tl = NA,
    ta_year_min = NULL,
    ta_year_max = NULL,
    Stratification = tryCatch(RoME::stratification_scheme, error = function(e) NULL),
    Ref_list = tryCatch(RoME::TM_list, error = function(e) NULL),
    DataTargetSpecies = tryCatch(RoME::DataTargetSpecies, error = function(e) NULL),
    Maturity = tryCatch(RoME::Maturity_parameters, error = function(e) NULL),
    ab_parameters = tryCatch(RoME::LW, error = function(e) NULL),
    stages_list = tryCatch(RoME::mat_stages, error = function(e) NULL),
    assTL = tryCatch(RoME::assTL, error = function(e) NULL)
  )

  output$status <- renderText({
    rv$status
  })

  output$run_log <- renderText({
    rv$log
  })

  output$year_info <- renderText({
    if (is.null(rv$ta_year_min) || is.null(rv$ta_year_max)) {
      "TA year range not available yet."
    } else {
      paste0("TA available range: ", rv$ta_year_min, " - ", rv$ta_year_max)
    }
  })

  observeEvent(input$browse_dir, {
    selected_dir <- tryCatch(
      svDialogs::dlg_dir(
        default = input$wd_run,
        title = "Choose results directory"
      )$res,
      error = function(e) NULL
    )

    if (!is.null(selected_dir) && !identical(selected_dir, "")) {
      updateTextInput(session, "wd_run", value = selected_dir)
    }
  })



  observe({
    if (isTRUE(rv$running)) {
      disable("run_btn")
    } else {
      enable("run_btn")
    }
  })

  observe({
    req(input$ta_file, input$tb_file, input$tc_file)

    ta <- try(read_medits_file(input$ta_file, input$sep, "TA"), silent = TRUE)
    tb <- try(read_medits_file(input$tb_file, input$sep, "TB"), silent = TRUE)
    tc <- try(read_medits_file(input$tc_file, input$sep, "TC"), silent = TRUE)

    if (inherits(ta, "try-error") || inherits(tb, "try-error") || inherits(tc, "try-error")) {
      rv$status <- "Error while reading one or more main input files."
      rv$log <- paste(
        c(
          if (inherits(ta, "try-error")) paste("TA:", as.character(ta)) else NULL,
          if (inherits(tb, "try-error")) paste("TB:", as.character(tb)) else NULL,
          if (inherits(tc, "try-error")) paste("TC:", as.character(tc)) else NULL
        ),
        collapse = "\n\n"
      )
      rv$ta <- NULL
      rv$tb <- NULL
      rv$tc <- NULL
      rv$ta_year_min <- NULL
      rv$ta_year_max <- NULL
      return(NULL)
    }

    rv$ta <- ta
    rv$tb <- tb
    rv$tc <- tc

    year_range <- get_year_range_from_ta(ta)

    if (!is.null(year_range)) {
      rv$ta_year_min <- year_range[1]
      rv$ta_year_max <- year_range[2]

      updateNumericInput(
        session,
        "year_from",
        value = year_range[1],
        min = year_range[1],
        max = year_range[2]
      )

      updateNumericInput(
        session,
        "year_to",
        value = year_range[2],
        min = year_range[1],
        max = year_range[2]
      )

      rv$status <- paste0(
        "Main files loaded. TA year range detected: ",
        year_range[1], " - ", year_range[2], "."
      )
    } else {
      rv$ta_year_min <- NULL
      rv$ta_year_max <- NULL
      rv$status <- "Main files loaded, but TA YEAR range could not be detected."
    }
  })

  observe({
    if (is.null(input$te_file)) {
      rv$te <- NA
    } else {
      te <- try(read_medits_file(input$te_file, input$sep, "TE"), silent = TRUE)
      if (inherits(te, "try-error")) {
        rv$status <- "Error while reading TE file."
        rv$log <- paste("TE:", as.character(te))
        rv$te <- NA
      } else {
        rv$te <- te
        rv$status <- "TE file loaded."
      }
    }
  })

  observe({
    if (is.null(input$tl_file)) {
      rv$tl <- NA
    } else {
      tl <- try(read_medits_file(input$tl_file, input$sep, "TL"), silent = TRUE)
      if (inherits(tl, "try-error")) {
        rv$status <- "Error while reading TL file."
        rv$log <- paste("TL:", as.character(tl))
        rv$tl <- NA
      } else {
        rv$tl <- tl
        rv$status <- "TL file loaded."
      }
    }
  })

  observe({
    if (!is.null(input$stratification_file)) {
      obj <- try(read_reference_object(input$stratification_file, input$aux_sep), silent = TRUE)
      if (inherits(obj, "try-error")) {
        rv$status <- "Error while reading custom Stratification."
      } else {
        rv$Stratification <- obj
        rv$status <- "Custom Stratification loaded."
      }
    }
  })

  observe({
    if (!is.null(input$ref_list_file)) {
      obj <- try(read_reference_object(input$ref_list_file, input$aux_sep), silent = TRUE)
      if (inherits(obj, "try-error")) {
        rv$status <- "Error while reading custom Ref_list."
      } else {
        rv$Ref_list <- obj
        rv$status <- "Custom Ref_list loaded."
      }
    }
  })

  observe({
    if (!is.null(input$target_species_file)) {
      obj <- try(read_reference_object(input$target_species_file, input$aux_sep), silent = TRUE)
      if (inherits(obj, "try-error")) {
        rv$status <- "Error while reading custom DataTargetSpecies."
      } else {
        rv$DataTargetSpecies <- obj
        rv$status <- "Custom DataTargetSpecies loaded."
      }
    }
  })

  observe({
    if (!is.null(input$maturity_file)) {
      obj <- try(read_reference_object(input$maturity_file, input$aux_sep), silent = TRUE)
      if (inherits(obj, "try-error")) {
        rv$status <- "Error while reading custom Maturity."
      } else {
        rv$Maturity <- obj
        rv$status <- "Custom Maturity loaded."
      }
    }
  })

  observe({
    if (!is.null(input$ab_parameters_file)) {
      obj <- try(read_reference_object(input$ab_parameters_file, input$aux_sep), silent = TRUE)
      if (inherits(obj, "try-error")) {
        rv$status <- "Error while reading custom ab_parameters."
      } else {
        rv$ab_parameters <- obj
        rv$status <- "Custom ab_parameters loaded."
      }
    }
  })

  observe({
    if (!is.null(input$stages_list_file)) {
      obj <- try(read_reference_object(input$stages_list_file, input$aux_sep), silent = TRUE)
      if (inherits(obj, "try-error")) {
        rv$status <- "Error while reading custom stages_list."
      } else {
        rv$stages_list <- obj
        rv$status <- "Custom stages_list loaded."
      }
    }
  })

  observe({
    if (!is.null(input$assTL_file)) {
      obj <- try(read_reference_object(input$assTL_file, input$aux_sep), silent = TRUE)
      if (inherits(obj, "try-error")) {
        rv$status <- "Error while reading custom assTL."
      } else {
        rv$assTL <- obj
        rv$status <- "Custom assTL loaded."
      }
    }
  })

  observeEvent(input$run_btn, {
    req(rv$ta, rv$tb, rv$tc)

    rv$status <- "Preparing analysis..."
    rv$log <- ""
    rv$zipfile <- NULL
    rv$run_dir <- NULL
    rv$running <- TRUE

    ta <- rv$ta
    tb <- rv$tb
    tc <- rv$tc
    te <- rv$te
    tl <- rv$tl

    year_range <- get_year_range_from_ta(ta)

    if (is.null(year_range)) {
      rv$status <- "Analysis failed."
      rv$log <- "ERROR: Unable to determine YEAR range from TA."
      rv$running <- FALSE
      return(NULL)
    }

    year_from <- suppressWarnings(as.integer(input$year_from))
    year_to <- suppressWarnings(as.integer(input$year_to))

    if (is.na(year_from) || is.na(year_to)) {
      rv$status <- "Analysis failed."
      rv$log <- "ERROR: Invalid year selection."
      rv$running <- FALSE
      return(NULL)
    }

    if (year_from > year_to) {
      rv$status <- "Analysis failed."
      rv$log <- "ERROR: First selected year is greater than last selected year."
      rv$running <- FALSE
      return(NULL)
    }

    if (year_from < year_range[1] || year_to > year_range[2]) {
      rv$status <- "Analysis failed."
      rv$log <- paste0(
        "ERROR: Selected years must stay within the TA range (",
        year_range[1], " - ", year_range[2], ")."
      )
      rv$running <- FALSE
      return(NULL)
    }

    ta_before <- nrow(ta)
    tb_before <- nrow(tb)
    tc_before <- nrow(tc)
    te_before <- if (is.data.frame(te)) nrow(te) else NA_integer_
    tl_before <- if (is.data.frame(tl)) nrow(tl) else NA_integer_

    ta <- filter_table_by_year(ta, year_from, year_to)
    tb <- filter_table_by_year(tb, year_from, year_to)
    tc <- filter_table_by_year(tc, year_from, year_to)
    te <- filter_table_by_year(te, year_from, year_to)
    tl <- filter_table_by_year(tl, year_from, year_to)

    if (nrow(ta) == 0) {
      rv$status <- "Analysis failed."
      rv$log <- "ERROR: TA is empty after year filtering."
      rv$running <- FALSE
      return(NULL)
    }

    if (nrow(tb) == 0) {
      rv$status <- "Analysis failed."
      rv$log <- "ERROR: TB is empty after year filtering."
      rv$running <- FALSE
      return(NULL)
    }

    if (nrow(tc) == 0) {
      rv$status <- "Analysis failed."
      rv$log <- "ERROR: TC is empty after year filtering."
      rv$running <- FALSE
      return(NULL)
    }

    prep <- apply_manual_rome_preprocessing(ta, tb, tc, te, tl)
    ta <- prep$ta
    tb <- prep$tb
    tc <- prep$tc
    te <- prep$te
    tl <- prep$tl

    suffix_val <- trimws(input$suffix)
    if (identical(suffix_val, "")) {
      suffix_val <- NA
    }

    wd_run <- trimws(input$wd_run)

    if (identical(wd_run, "")) {
      rv$status <- "Analysis failed."
      rv$log <- "ERROR: Results directory is empty."
      rv$running <- FALSE
      return(NULL)
    }

    dir.create(wd_run, recursive = TRUE, showWarnings = FALSE)
    rv$run_dir <- wd_run

    rv$status <- paste("Running", input$rome_fun, "...")

    if (is.null(rv$Stratification)) {
      rv$status <- "Analysis failed."
      rv$log <- "ERROR: Stratification is NULL."
      rv$running <- FALSE
      return(NULL)
    }
    if (is.null(rv$Ref_list)) {
      rv$status <- "Analysis failed."
      rv$log <- "ERROR: Ref_list is NULL."
      rv$running <- FALSE
      return(NULL)
    }
    if (is.null(rv$DataTargetSpecies)) {
      rv$status <- "Analysis failed."
      rv$log <- "ERROR: DataTargetSpecies is NULL."
      rv$running <- FALSE
      return(NULL)
    }
    if (is.null(rv$Maturity)) {
      rv$status <- "Analysis failed."
      rv$log <- "ERROR: Maturity is NULL."
      rv$running <- FALSE
      return(NULL)
    }
    if (is.null(rv$ab_parameters)) {
      rv$status <- "Analysis failed."
      rv$log <- "ERROR: ab_parameters is NULL."
      rv$running <- FALSE
      return(NULL)
    }
    if (is.null(rv$stages_list)) {
      rv$status <- "Analysis failed."
      rv$log <- "ERROR: stages_list is NULL."
      rv$running <- FALSE
      return(NULL)
    }
    if (is.null(rv$assTL)) {
      rv$status <- "Analysis failed."
      rv$log <- "ERROR: assTL is NULL."
      rv$running <- FALSE
      return(NULL)
    }

    result <- tryCatch({
      fun_to_run <- switch(
        input$rome_fun,
        "RoMEcc" = try(RoME::RoMEcc, silent = TRUE),
        "RoMEBScc" = try(RoME::RoMEBScc, silent = TRUE)
      )

      if (inherits(fun_to_run, "try-error") || is.null(fun_to_run)) {
        stop(paste("Function", input$rome_fun, "is not available in the loaded RoME package/environment."))
      }

      log_output <- capture.output(
        res <- fun_to_run(
          TA = ta,
          TB = tb,
          TC = tc,
          TE = te,
          TL = tl,
          wd = wd_run,
          suffix = suffix_val,
          verbose = isTRUE(input$verbose),
          Stratification = rv$Stratification,
          Ref_list = rv$Ref_list,
          DataTargetSpecies = rv$DataTargetSpecies,
          Maturity = rv$Maturity,
          ab_parameters = rv$ab_parameters,
          stages_list = rv$stages_list,
          assTL = rv$assTL,
          zip = isTRUE(input$zip)
        ),
        type = "output"
      )

      list(
        success = TRUE,
        log_output = log_output,
        result_object = res
      )

    }, error = function(e) {
      list(
        success = FALSE,
        error_message = conditionMessage(e)
      )
    })

    if (isTRUE(result$success)) {
      zip_candidates <- list.files(
        path = wd_run,
        pattern = "\\.zip$",
        recursive = TRUE,
        full.names = TRUE
      )

      summary_candidates <- list.files(
        path = wd_run,
        pattern = "^ERRORS_summary_.*\\.csv$",
        recursive = TRUE,
        full.names = TRUE
      )

      logfile_candidates <- list.files(
        path = file.path(wd_run, "Logfiles"),
        pattern = "\\.dat$",
        recursive = TRUE,
        full.names = TRUE
      )

      final_log <- result$log_output

      final_log <- c(
        final_log,
        "",
        paste("Function used:", input$rome_fun),
        paste("Working directory:", wd_run),
        paste("ZIP requested:", isTRUE(input$zip)),
        paste("Selected year range:", paste0(year_from, " - ", year_to)),
        paste("TA rows before/after filter:", paste(ta_before, nrow(ta), sep = " / ")),
        paste("TB rows before/after filter:", paste(tb_before, nrow(tb), sep = " / ")),
        paste("TC rows before/after filter:", paste(tc_before, nrow(tc), sep = " / "))
      )

      if (!is.na(te_before) && is.data.frame(te)) {
        final_log <- c(
          final_log,
          paste("TE rows before/after filter:", paste(te_before, nrow(te), sep = " / "))
        )
      }

      if (!is.na(tl_before) && is.data.frame(tl)) {
        final_log <- c(
          final_log,
          paste("TL rows before/after filter:", paste(tl_before, nrow(tl), sep = " / "))
        )
      }

      if (length(summary_candidates) > 0) {
        final_log <- c(final_log, paste("Summary CSV:", summary_candidates[1]))
      }

      if (length(logfile_candidates) > 0) {
        final_log <- c(final_log, paste("Log file:", logfile_candidates[1]))
      }

      if (length(zip_candidates) > 0) {
        rv$zipfile <- zip_candidates[1]
        final_log <- c(
          final_log,
          paste("ZIP file created:", zip_candidates[1]),
          "",
          "Run completed successfully."
        )
      } else {
        rv$zipfile <- NULL
        final_log <- c(
          final_log,
          "",
          "Run completed successfully, but no ZIP file was found."
        )
      }

      rv$log <- paste(final_log, collapse = "\n")
      rv$status <- paste(input$rome_fun, "completed.")

    } else {
      rv$zipfile <- NULL
      rv$log <- paste("ERROR:", result$error_message)
      rv$status <- "Analysis failed."
    }

    rv$running <- FALSE
  })

  output$download_results <- downloadHandler(
    filename = function() {
      zipfile <- rv$zipfile
      if (!is.null(zipfile) && file.exists(zipfile)) {
        basename(zipfile)
      } else {
        paste0(input$rome_fun, "_results.zip")
      }
    },
    content = function(file) {
      zipfile <- rv$zipfile
      req(!is.null(zipfile))
      req(file.exists(zipfile))
      ok <- file.copy(zipfile, file, overwrite = TRUE)
      if (!isTRUE(ok)) {
        stop("Unable to copy the ZIP file for download.")
      }
    }
  )




}

shinyApp(ui, server)
