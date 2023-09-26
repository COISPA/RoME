

# TEST INIZIALIZING --------------------------------------------------------
if (FALSE) {
  library(RoME)
  wd <- "D:\\OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L\\RDB\\RoME_RDBFIS\\check_RoME\\files_to_checks" # tempdir()
  suffix <- NA
  TA <- DataTA # read.table(file = paste(wd, "\\TA.csv", sep = ""), sep = ";", header = T) # RoME::TA
  TB <- DataTB # read.table(file = paste(wd, "\\TB.csv", sep = ""), sep = ";", header = T) # RoME::TB
  TC <- DataTC # read.table(file = paste(wd, "\\TC.csv", sep = ""), sep = ";", header = T) # RoME::TC
  TE <- NA # read.table(file=paste(wd, "\\TE.csv",sep=""), sep=";", header=T)
  TL <- NA # read.table(file=paste(wd, "\\TL.csv",sep=""), sep=";", header=T) # RoME::TL
  verbose <- TRUE
  create_RSufi_files <- TRUE
  create_global_RSufi_files <- TRUE
  Year_start <- 2012
  Year_end <- 2012

  # TB$TYPE_OF_FILE <- as.character(TB$TYPE_OF_FILE)
  # TB$TYPE_OF_FILE[1] <- "TC"

  RoME(TA = RoME::TA, TB = TB, TC = TC, TE = TE, TL = TL, wd = wd, suffix = NA, create_RSufi_files = TRUE, create_global_RSufi_files = TRUE, Year_start = 1994, Year_end = 2020, verbose = TRUE)

  RoME(TA = DataTA, TB = DataTB, TC = DataTC, TE = TE, TL = TL, wd = wd, suffix = NA, create_RSufi_files = TRUE, create_global_RSufi_files = TRUE, Year_start = 1994, Year_end = 2020, verbose = TRUE)

}
# TEST END --------------------------------------------------------


RoME <- function(TA, TB, TC, TE = NA, TL = NA, wd, suffix = NA, create_RSufi_files = FALSE, create_global_RSufi_files = FALSE, Year_start = NA, Year_end = NA, verbose = TRUE, Stratification = RoME::stratification_scheme, Ref_list = RoME::TM_list, DataTargetSpecies = RoME::DataTargetSpecies, Maturity = RoME::Maturity_parameters, ab_parameters = RoME::LW, stages_list = RoME::mat_stages, assTL = assTL) {
  stringsAsFactors <- FALSE
  Format <- "from_2012"

  if (!file.exists(file.path(wd, "Logfiles"))) {
    dir.create(file.path(wd, "Logfiles"), recursive = TRUE, showWarnings = FALSE)
  }
  if (!file.exists(file.path(wd, "Graphs"))) {
    dir.create(file.path(wd, "Graphs"), showWarnings = FALSE)
  }
  if (!file.exists(file.path(wd, "/files R-Sufi", sep = "/"))) {
    dir.create(file.path(wd, "files R-Sufi"), showWarnings = FALSE)
  }

  if (is.na(suffix)) {
    suffix <- paste(as.character(Sys.Date()), format(Sys.time(), "_time_h%Hm%Ms%OS0"), sep = "")
  }
  Errors <- file.path(wd, "Logfiles", paste("Logfile_", suffix, ".dat", sep = ""))
  if (!file.exists(Errors)) {
    file.create(Errors)
  }
  write(paste("\n ", date(), sep = ""), file = Errors, append = TRUE)


  stop_ <- FALSE
  check_without_errors <- TRUE

  # START -------------------------------------------------------------------

  write(paste("-------------------------------------------------------------\n
              LIST OF ERRORS
              \n-------------------------------------------------------------"), file = Errors, append = TRUE)


  # check degli header ------------------------------------------------------
  table <- "TA"
  checkName <- "Check Headers"
  if (check_without_errors == TRUE) {
    if (verbose) {
      print(paste(checkName,table, "in progress..."), quote = FALSE)
    }
    check_without_errors <- checkHeader(TA, "TA", wd, suffix)
  }
  if (verbose) {
    stop_ <- printError(checkName, check_without_errors, stop_)
  }


  table <- "TB"
  checkName <- "Check Headers"
  if (check_without_errors == TRUE) {
    if (verbose) {
      print(paste(checkName,table, "in progress..."), quote = FALSE)
    }
    check_without_errors <- checkHeader(TB, "TB", wd, suffix)
  }
  if (verbose) {
    stop_ <- printError(checkName, check_without_errors, stop_)
  }


  table <- "TC"
  checkName <- "Check Headers"
  if (check_without_errors == TRUE) {
    if (verbose) {
      print(paste(checkName,table, "in progress..."), quote = FALSE)
    }
    check_without_errors <- checkHeader(TC, "TC", wd, suffix)
  }
  if (verbose) {
    stop_ <- printError(checkName, check_without_errors, stop_)
  }

  ResultDataTA_bkp <- TA
  ResultDataTB_bkp <- TB
  ResultDataTC_bkp <- TC


  ###  TE  ###
  if (!(all(is.na(TE)) & length(TE) == 1)) {
    table <- "TE"
    checkName <- "Check Headers"
    if (check_without_errors == TRUE) {
      if (verbose) {
        print(paste(checkName,table, "in progress..."), quote = FALSE)
      }
      check_without_errors <- checkHeader(TE, "TE", wd, suffix)
    }
    if (verbose) {
      stop_ <- printError(checkName, check_without_errors, stop_)
    }

    ResultDataTE_bkp <- TE
  }

  ###  TL  ###
  if (!(all(is.na(TL)) & length(TL) == 1)) {
    table <- "TL"
    checkName <- "Check Headers"
    if (check_without_errors == TRUE) {
      if (verbose) {
        print(paste(checkName,table, "in progress..."), quote = FALSE)
      }
      check_without_errors <- checkHeader(TL, "TL", wd, suffix)
    }
    if (verbose) {
      stop_ <- printError(checkName, check_without_errors, stop_)
    }

    ResultDataTL_bkp <- TL
  }
  ###########

  ### CHECK CLASS
  table <- "TA"
  checkName <- "Check CLASS"
  if (check_without_errors == TRUE) {
    if (verbose) {
      print(paste(checkName,table, "in progress..."), quote = FALSE)
    }
    check_without_errors <- check_class(data = TA, tab = "TA", wd = wd, suffix = suffix)
  }
  if (verbose) {
    stop_ <- printError(checkName, check_without_errors, stop_)
  }


  table <- "TB"
  checkName <- "Check CLASS"
  if (check_without_errors == TRUE) {
    if (verbose) {
      print(paste(checkName,table, "in progress..."), quote = FALSE)
    }
    check_without_errors <- check_class(data = TB, tab = "TB", wd = wd, suffix = suffix)
  }
  if (verbose) {
    stop_ <- printError(checkName, check_without_errors, stop_)
  }


  table <- "TC"
  checkName <- "Check CLASS"
  if (check_without_errors == TRUE) {
    if (verbose) {
      print(paste(checkName,table, "in progress..."), quote = FALSE)
    }
    check_without_errors <- check_class(data = TC, tab = "TC", wd = wd, suffix = suffix)
  }
  if (verbose) {
    stop_ <- printError(checkName, check_without_errors, stop_)
  }



  if (!(all(is.na(TE)) & length(TE) == 1)) {
    table <- "TE"
    checkName <- "Check CLASS"
    if (check_without_errors == TRUE) {
      if (verbose) {
        print(paste(checkName,table, "in progress..."), quote = FALSE)
      }
      check_without_errors <- check_class(data = TE, tab = "TE", wd = wd, suffix = suffix)
    }
    if (verbose) {
      stop_ <- printError(checkName, check_without_errors, stop_)
    }
  }


  if (!(all(is.na(TL)) & length(TL) == 1)) {
    table <- "TL"
    checkName <- "Check CLASS"
    if (check_without_errors == TRUE) {
      if (verbose) {
        print(paste(checkName,table, "in progress..."), quote = FALSE)
      }
      check_without_errors <- check_class(data = TL, tab = "TL", wd = wd, suffix = suffix)
    }
    if (verbose) {
      stop_ <- printError(checkName, check_without_errors, stop_)
    }
  }



  ### CHECK YEAR

  years <- sort(unique(TA$YEAR))

  checkName <- "Check YEAR"
  if (check_without_errors == TRUE) {
    if (verbose) {
      print(paste(checkName, "in progress..."), quote = FALSE)
    }
    check_without_errors <- check_year(TA, TB, TC, TE, TL, years, wd, Errors)
  }
  if (verbose) {
    stop_ <- printError(checkName, check_without_errors, stop_)
  }
  ### CHECK TYPE_OF_FILE

  checkName <- "Check TYPE_OF_FILE"
  if (check_without_errors == TRUE) {
    if (verbose) {
      print(paste(checkName, "in progress..."), quote = FALSE)
    }
    check_without_errors <- check_type(TA, TB, TC, TE, TL, years, wd, Errors)
  }
  if (verbose) {
    stop_ <- printError(checkName, check_without_errors, stop_)
  }



  ## CICLO PER ANNO ##

  yea <- years[1]
  for (yea in years) {

    cat(paste0("\n########################"))
    cat(paste0("\n###       ",yea,"       ###"))
    cat(paste0("\n########################"))

    if (check_without_errors == TRUE & verbose == TRUE) {
      print(paste("Checking year ", yea), quote = F)
    }

    #------------------

    ResultDataTA <- ResultDataTA_bkp[ResultDataTA_bkp$YEAR == yea, ]
    ResultDataTB <- ResultDataTB_bkp[ResultDataTB_bkp$YEAR == yea, ]
    ResultDataTC <- ResultDataTC_bkp[ResultDataTC_bkp$YEAR == yea, ]

    if (!(all(is.na(TE)) & length(TE) == 1)) {
      ResultDataTE <- ResultDataTE_bkp[ResultDataTE_bkp$YEAR == yea, ]
    } else {
      ResultDataTE <- NA
    }

    if (!(all(is.na(TL)) & length(TL) == 1)) {
      ResultDataTL <- ResultDataTL_bkp[ResultDataTL_bkp$YEAR == yea, ]
    } else {
      ResultDataTL <- NA
    }

    # --------------------------------
    # Check identical records

    checkName <- "Check identical record TA"
    if (check_without_errors == TRUE) {
      if (verbose) {
        print(paste(checkName, "in progress..."), quote = FALSE)
      }
      check_without_errors <- check_identical_records(Data = ResultDataTA, year = yea, wd, suffix)
    }
    if (verbose) {
      stop_ <- printError(checkName, check_without_errors, stop_)
    }

    checkName <- "Check identical record TB"
    if (check_without_errors == TRUE) {
      if (verbose) {
        print(paste(checkName, "in progress..."), quote = FALSE)
      }
      check_without_errors <- check_identical_records(Data = ResultDataTB, year = yea, wd, suffix)
    }
    if (verbose) {
      stop_ <- printError(checkName, check_without_errors, stop_)
    }

    checkName <- "Check identical record TC"
    if (check_without_errors == TRUE) {
      if (verbose) {
        print(paste(checkName, "in progress..."), quote = FALSE)
      }
      check_without_errors <- check_identical_records(Data = ResultDataTC, year = yea, wd, suffix)
    }
    if (verbose) {
      stop_ <- printError(checkName, check_without_errors, stop_)
    }

    if (!(all(is.na(TE)) & length(TE) == 1)) {
      if (nrow(ResultDataTE) > 0) {
        checkName <- "Check identical record TE"
        if (check_without_errors == TRUE) {
          if (verbose) {
            print(paste(checkName, "in progress..."), quote = FALSE)
          }
          check_without_errors <- check_identical_records(Data = ResultDataTE, year = yea, wd, suffix)
        }
        if (verbose) {
          stop_ <- printError(checkName, check_without_errors, stop_)
        }
      }
    }

    if (!(all(is.na(TL)) & length(TL) == 1)) {
      if (nrow(ResultDataTL) > 0) {
        checkName <- "Check identical record TL"
        if (check_without_errors == TRUE) {
          if (verbose) {
            print(paste(checkName, "in progress..."), quote = FALSE)
          }
          check_without_errors <- check_identical_records(Data = ResultDataTL, year = yea, wd, suffix)
        }
        if (verbose) {
          stop_ <- printError(checkName, check_without_errors, stop_)
        }
      }
    }


    # --------------------------------
    # Check quasi-identical record

    checkName <- "Check quasi-identical record in TA"
    if (check_without_errors == TRUE) {
      if (verbose) {
        print(paste(checkName, "in progress..."), quote = FALSE)
      }
      check_without_errors <- check_quasiidentical_records(ResultDataTA, year = yea, wd, suffix)
    }
    if (verbose) {
      stop_ <- printError(checkName, check_without_errors, stop_)
    }


    checkName <- "Check quasi-identical record in TB"
    if (check_without_errors == TRUE) {
      if (verbose) {
        print(paste(checkName, "in progress..."), quote = FALSE)
      }
      check_without_errors <- check_quasiidentical_records(ResultDataTB, year = yea, wd, suffix)
    }
    if (verbose) {
      stop_ <- printError(checkName, check_without_errors, stop_)
    }


    checkName <- "Check quasi-identical record in TC"
    if (check_without_errors == TRUE) {
      if (verbose) {
        print(paste(checkName, "in progress..."), quote = FALSE)
      }
      check_without_errors <- check_quasiidentical_records(ResultDataTC, year = yea, wd, suffix)
    }
    if (verbose) {
      stop_ <- printError(checkName, check_without_errors, stop_)
    }


    if (!(all(is.na(TE)) & length(TE) == 1)) {
      if (nrow(ResultDataTE) > 0) {
        checkName <- "Check quasi-identical record in TE"
        if (check_without_errors == TRUE) {
          if (verbose) {
            print(paste(checkName, "in progress..."), quote = FALSE)
          }
          check_without_errors <- check_quasiidentical_records(ResultDataTE, year = yea, wd, suffix)
        }
        if (verbose) {
          stop_ <- printError(checkName, check_without_errors, stop_)
        }
      }
    }

    if (!(all(is.na(TL)) & length(TL) == 1)) {
      if (nrow(ResultDataTL) > 0) {
        checkName <- "Check quasi-identical record in TL"
        if (check_without_errors == TRUE) {
          if (verbose) {
            print(paste(checkName, "in progress..."), quote = FALSE)
          }
          check_without_errors <- check_quasiidentical_records(ResultDataTL, year = yea, wd, suffix)
        }
        if (verbose) {
          stop_ <- printError(checkName, check_without_errors, stop_)
        }
      }
    }


    # --------------------------------
    # Check consistency of area

    checkName <- "Check consistency of area TA, TB, TC, TE, TL"
    if (check_without_errors == TRUE) {
      if (verbose) {
        print(paste(checkName, "in progress..."), quote = FALSE)
      }
      check_without_errors <- suppressWarnings(check_area(ResultDataTA, ResultDataTB, ResultDataTC, ResultDataTE, ResultDataTL, year = yea, wd, suffix))
    }
    if (verbose) {
      stop_ <- printError(checkName, check_without_errors, stop_)
    }


    #--------------------------------------------------
    # Dictionary checks
    #--------------------------------------------------

    # TA

    checkName <- "Check dictionary for field:"
    Field <- "COUNTRY"
    Values <- as.character(unique(Stratification$COUNTRY))
    if (check_without_errors == TRUE) {
      if (verbose) {
        print(paste(checkName, Field, "in progress..."), quote = FALSE)
      }
      check_without_errors <- check_dictionary(ResultData = ResultDataTA, Field, Values, year = yea, wd, suffix)
    }
    if (verbose) {
      stop_ <- printError(paste(checkName, Field), check_without_errors, stop_)
    }


    checkName <- "Check dictionary for field:"
    Field <- "AREA"
    Values <- as.character(unique(Stratification$GSA))
    if (check_without_errors == TRUE) {
      if (verbose) {
        print(paste(checkName, Field, "in progress..."), quote = FALSE)
      }
      check_without_errors <- check_dictionary(ResultData = ResultDataTA, Field, Values, year = yea, wd, suffix)
    }
    if (verbose) {
      stop_ <- printError(paste(checkName, Field), check_without_errors, stop_)
    }


    Field <- "GEAR"
    Values <- c("GOC73")
    if (check_without_errors == TRUE) {
      if (verbose) {
        print(paste(checkName, Field, "in progress..."), quote = FALSE)
      }
      check_without_errors <- check_dictionary(ResultData = ResultDataTA, Field, Values, year = yea, wd, suffix)
    }
    if (verbose) {
      stop_ <- printError(paste(checkName, Field), check_without_errors, stop_)
    }

    Field <- "RIGGING"
    Values <- c("GC73")
    if (check_without_errors == TRUE) {
      if (verbose) {
        print(paste(checkName, Field, "in progress..."), quote = FALSE)
      }
      check_without_errors <- check_dictionary(ResultData = ResultDataTA, Field, Values, year = yea, wd, suffix)
    }
    if (verbose) {
      stop_ <- printError(paste(checkName, Field), check_without_errors, stop_)
    }

    Field <- "DOORS"
    Values <- c("WHS8")
    if (check_without_errors == TRUE) {
      if (verbose) {
        print(paste(checkName, Field, "in progress..."), quote = FALSE)
      }
      check_without_errors <- check_dictionary(ResultData = ResultDataTA, Field, Values, year = yea, wd, suffix)
    }
    if (verbose) {
      stop_ <- printError(paste(checkName, Field), check_without_errors, stop_)
    }

    Field <- "MONTH"
    Values <- seq(1, 12, 1)
    if (check_without_errors == TRUE) {
      if (verbose) {
        print(paste(checkName, Field, "in progress..."), quote = FALSE)
      }
      check_without_errors <- check_dictionary(ResultData = ResultDataTA, Field, Values, year = yea, wd, suffix)
    }
    if (verbose) {
      stop_ <- printError(paste(checkName, Field), check_without_errors, stop_)
    }


    Field <- "DAY"
    Values <- seq(1, 31, 1)
    if (check_without_errors == TRUE) {
      if (verbose) {
        print(paste(checkName, Field, "in progress..."), quote = FALSE)
      }
      check_without_errors <- check_dictionary(ResultData = ResultDataTA, Field, Values, year = yea, wd, suffix)
    }
    if (verbose) {
      stop_ <- printError(paste(checkName, Field), check_without_errors, stop_)
    }


    Field <- "HAUL_NUMBER"
    Values = seq(1,999,1)
    if (check_without_errors == TRUE) {
      if (verbose) {
        print(paste(checkName, Field, "in progress..."), quote = FALSE)
      }
      check_without_errors <- check_dictionary(ResultData = ResultDataTA, Field, Values, year = yea, wd, suffix)
    }
    if (verbose) {
      stop_ <- printError(paste(checkName, Field), check_without_errors, stop_)
    }


    Field <- "CODEND_CLOSING"
    Values <- c("S", "C")
    if (check_without_errors == TRUE) {
      if (verbose) {
        print(paste(checkName, Field, "in progress..."), quote = FALSE)
      }
      check_without_errors <- check_dictionary(ResultData = ResultDataTA, Field, Values, year = yea, wd, suffix)
    }
    if (verbose) {
      stop_ <- printError(paste(checkName, Field), check_without_errors, stop_)
    }


    Field <- "PART_OF_THE_CODEND"
    Values <- c("A", "M", "P", "S")
    if (check_without_errors == TRUE) {
      if (verbose) {
        print(paste(checkName, Field, "in progress..."), quote = FALSE)
      }
      check_without_errors <- check_dictionary(ResultData = ResultDataTA, Field, Values, year = yea, wd, suffix)
    }
    if (verbose) {
      stop_ <- printError(paste(checkName, Field), check_without_errors, stop_)
    }


    Field = "SHOOTING_TIME"
    Values = seq(0,2400,1)
    if (check_without_errors == TRUE) {
      if(verbose){
        print(paste(checkName,Field,"in progress..."), quote = FALSE)
        }
      check_without_errors = check_dictionary(ResultData = ResultDataTA, Field, Values, year=yea, wd, suffix)
    }
    if(verbose){
      stop_ = printError(paste(checkName,Field),check_without_errors, stop_)
    }


    Field <- "SHOOTING_QUADRANT"
    Values <- c("1", "3", "5", "7")
    if (check_without_errors == TRUE) {
      if (verbose) {
        print(paste(checkName, Field, "in progress..."), quote = FALSE)
      }
      check_without_errors <- check_dictionary(ResultData = ResultDataTA, Field, Values, year = yea, wd, suffix)
    }
    if (verbose) {
      stop_ <- printError(paste(checkName, Field), check_without_errors, stop_)
    }


    Field = "SHOOTING_LATITUDE"
    Values = c(3400,4600)
    if (check_without_errors == TRUE) {
      if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
      check_without_errors = check_numeric_range(Data = ResultDataTA, Field, Values, year=yea, wd, suffix)
    }
    if(verbose){stop_ = printError(paste(checkName,Field),check_without_errors, stop_)}



    Field = "SHOOTING_LONGITUDE"
    Values = c(0,3500)
    if (check_without_errors == TRUE) {
      if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
      check_without_errors = check_numeric_range(Data = ResultDataTA, Field, Values, year=yea, wd, suffix)
    }
    if(verbose){stop_ = printError(paste(checkName,Field),check_without_errors, stop_)}



    Field <- "SHOOTING_DEPTH"
    Values <- c(0, 10:800)
    if (check_without_errors == TRUE) {
      if (verbose) {
        print(paste(checkName, Field, "in progress..."), quote = FALSE)
      }
      check_without_errors <- check_dictionary(ResultData = ResultDataTA, Field, Values, year = yea, wd, suffix)
    }
    if (verbose) {
      stop_ <- printError(paste(checkName, Field), check_without_errors, stop_)
    }


    Field <- "HAULING_QUADRANT"
    Values <- c("1", "3", "5", "7")
    if (check_without_errors == TRUE) {
      if (verbose) {
        print(paste(checkName, Field, "in progress..."), quote = FALSE)
      }
      check_without_errors <- check_dictionary(ResultData = ResultDataTA, Field, Values, year = yea, wd, suffix)
    }
    if (verbose) {
      stop_ <- printError(paste(checkName, Field), check_without_errors, stop_)
    }


    Field = "HAULING_LATITUDE"
    Values = c(3400,4600)
    if (check_without_errors == TRUE) {
      if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
      check_without_errors = check_numeric_range(Data = ResultDataTA, Field, Values, year=yea, wd, suffix)
    }
    if(verbose){stop_ = printError(paste(checkName,Field),check_without_errors, stop_)}



    Field = "HAULING_LONGITUDE"
    Values = c(0,3500)
    if (check_without_errors == TRUE) {
      if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
      check_without_errors = check_numeric_range(Data = ResultDataTA, Field, Values, year=yea, wd, suffix)
    }
    if(verbose){stop_ = printError(paste(checkName,Field),check_without_errors, stop_)}



    Field <- "HAULING_TIME"
    Values <- seq(0, 2400, 1)
    if (check_without_errors == TRUE) {
      if (verbose) {
        print(paste(checkName, Field, "in progress..."), quote = FALSE)
      }
      check_without_errors <- check_dictionary(ResultData = ResultDataTA, Field, Values, year = yea, wd, suffix)
    }
    if (verbose) {
      stop_ <- printError(paste(checkName, Field), check_without_errors, stop_)
    }

    Field <- "HAULING_DEPTH"
    Values <- c(0, 10:800)
    if (check_without_errors == TRUE) {
      if (verbose) {
        print(paste(checkName, Field, "in progress..."), quote = FALSE)
      }
      check_without_errors <- check_dictionary(ResultData = ResultDataTA, Field, Values, year = yea, wd, suffix)
    }
    if (verbose) {
      stop_ <- printError(paste(checkName, Field), check_without_errors, stop_)
    }


    Field = "HAUL_DURATION"
    Values = c(5:90)
    if (check_without_errors == TRUE) {
      if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
      check_without_errors = check_dictionary(ResultData = ResultDataTA, Field, Values, year=yea, wd, suffix)
    }
    if(verbose){stop_ = printError(paste(checkName,Field),check_without_errors, stop_)}



    Field <- "VALIDITY"
    Values <- c("V", "I")
    if (check_without_errors == TRUE) {
      if (verbose) {
        print(paste(checkName, Field, "in progress..."), quote = FALSE)
      }
      check_without_errors <- check_dictionary(ResultData = ResultDataTA, Field, Values, year = yea, wd, suffix)
    }
    if (verbose) {
      stop_ <- printError(paste(checkName, Field), check_without_errors, stop_)
    }

    Field <- "COURSE"
    Values <- c("R", "N")
    if (check_without_errors == TRUE) {
      if (verbose) {
        print(paste(checkName, Field, "in progress..."), quote = FALSE)
      }
      check_without_errors <- check_dictionary(ResultData = ResultDataTA, Field, Values, year = yea, wd, suffix)
    }
    if (verbose) {
      stop_ <- printError(paste(checkName, Field), check_without_errors, stop_)
    }


    Field = "DISTANCE"
    Values = c(1000:9999)
    if (check_without_errors == TRUE) {
      if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
      check_without_errors = check_dictionary(ResultData = ResultDataTA, Field, Values, year=yea, wd, suffix)
    }
    if(verbose){stop_ = printError(paste(checkName,Field),check_without_errors, stop_)}



    Field = "VERTICAL_OPENING"
    Values = c(10:100)
    if (check_without_errors == TRUE) {
      if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
      check_without_errors = check_dictionary(ResultData = ResultDataTA, Field, Values, year=yea, wd, suffix)
    }
    if(verbose){stop_ = printError(paste(checkName,Field),check_without_errors, stop_)}


    Field = "WING_OPENING"
    Values = c(50:250)
    if (check_without_errors == TRUE) {
      if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
      check_without_errors = check_dictionary(ResultData = ResultDataTA, Field, Values, year=yea, wd, suffix)
    }
    if(verbose){stop_ = printError(paste(checkName,Field),check_without_errors, stop_)}



    Field <- "GEOMETRICAL_PRECISION"
    Values <- c("M", "E")
    if (check_without_errors == TRUE) {
      if (verbose) {
        print(paste(checkName, Field, "in progress..."), quote = FALSE)
      }
      check_without_errors <- check_dictionary(ResultData = ResultDataTA, Field, Values, year = yea, wd, suffix)
    }
    if (verbose) {
      stop_ <- printError(paste(checkName, Field), check_without_errors, stop_)
    }



    Field <- "MEASURING_SYSTEM"
    Values <- c("VA", "SO", "XA", "SA", "SI", "CT", "SB",NA)
    if (check_without_errors == TRUE) {
      if (verbose) {
        print(paste(checkName, Field, "in progress..."), quote = FALSE)
      }
      check_without_errors <- check_dictionary(ResultData = ResultDataTA, Field, Values, year = yea, wd, suffix)
    }
    if (verbose) {
      stop_ <- printError(paste(checkName, Field), check_without_errors, stop_)
    }

    Field <- "RECORDED_SPECIES"
    Values <- seq(0, 4, 1)
    if (check_without_errors == TRUE) {
      if (verbose) {
        print(paste(checkName, Field, "in progress..."), quote = FALSE)
      }
      check_without_errors <- check_dictionary(ResultData = ResultDataTA, Field, Values, year = yea, wd, suffix)
    }
    if (verbose) {
      stop_ <- printError(paste(checkName, Field), check_without_errors, stop_)
    }

    Field <- "WARP_LENGTH"
    Values <- seq(100, 2200, 1)
    if (check_without_errors == TRUE) {
      if (verbose) {
        print(paste(checkName, Field, "in progress..."), quote = FALSE)
      }
      check_without_errors <- check_dictionary(ResultData = ResultDataTA, Field, Values, year = yea, wd, suffix)
    }
    if (verbose) {
      stop_ <- printError(paste(checkName, Field), check_without_errors, stop_)
    }

    Field <- "WARP_DIAMETER"
    Values <- seq(10, 30, 1)
    if (check_without_errors == TRUE) {
      if (verbose) {
        print(paste(checkName, Field, "in progress..."), quote = FALSE)
      }
      check_without_errors <- check_dictionary(ResultData = ResultDataTA, Field, Values, year = yea, wd, suffix)
    }
    if (verbose) {
      stop_ <- printError(paste(checkName, Field), check_without_errors, stop_)
    }

    Field <- "OBSERVATIONS"
    Values <- seq(0, 9, 1)
    if (check_without_errors == TRUE) {
      if (verbose) {
        print(paste(checkName, Field, "in progress..."), quote = FALSE)
      }
      check_without_errors <- check_dictionary(ResultData = ResultDataTA, Field, Values, year = yea, wd, suffix)
    }
    if (verbose) {
      stop_ <- printError(paste(checkName, Field), check_without_errors, stop_)
    }



    Field = "BOTTOM_TEMPERATURE_BEGINNING"
    Values = c(0,30)
    if (check_without_errors == TRUE) {
      if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
      check_without_errors = check_numeric_range(Data = ResultDataTA, Field, Values, year=yea, wd, suffix)
    }
    if(verbose){stop_ = printError(paste(checkName,Field),check_without_errors, stop_)}


    Field = "BOTTOM_TEMPERATURE_END"
    Values = c(0,30)
    if (check_without_errors == TRUE) {
      if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
      check_without_errors = check_numeric_range(Data = ResultDataTA, Field, Values, year=yea, wd, suffix)
    }
    if(verbose){stop_ = printError(paste(checkName,Field),check_without_errors, stop_)}


    Field = "BOTTOM_SALINITY_BEGINNING"
    Values = c(0,50)
    if (check_without_errors == TRUE) {
      if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
      check_without_errors = check_numeric_range(Data = ResultDataTA, Field, Values, year=yea, wd, suffix)
    }
    if(verbose){stop_ = printError(paste(checkName,Field),check_without_errors, stop_)}



    Field = "BOTTOM_SALINITY_END"
    Values = c(0,50)
    if (check_without_errors == TRUE) {
      if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
      check_without_errors = check_numeric_range(Data = ResultDataTA, Field, Values, year=yea, wd, suffix)
    }
    if(verbose){stop_ = printError(paste(checkName,Field),check_without_errors, stop_)}



    # TB
    Field <- "COUNTRY"
    Values <- as.character(unique(Stratification$COUNTRY))
    if (check_without_errors == TRUE) {
      if (verbose) {
        print(paste(checkName, Field, "in progress..."), quote = FALSE)
      }
      check_without_errors <- check_dictionary(ResultData = ResultDataTB, Field, Values, year = yea, wd, suffix)
    }
    if (verbose) {
      stop_ <- printError(paste(checkName, Field), check_without_errors, stop_)
    }



    Field = "AREA"
    Values = as.character(unique(Stratification$GSA))
    if (check_without_errors == TRUE) {
      if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
      check_without_errors = check_dictionary(ResultData = ResultDataTA, Field, Values, year=yea, wd, suffix)
    }
    if(verbose){stop_ = printError(paste(checkName,Field),check_without_errors, stop_)}



    Field <- "MONTH"
    Values <- seq(1, 12, 1)
    if (check_without_errors == TRUE) {
      if (verbose) {
        print(paste(checkName, Field, "in progress..."), quote = FALSE)
      }
      check_without_errors <- check_dictionary(ResultData = ResultDataTB, Field, Values, year = yea, wd, suffix)
    }
    if (verbose) {
      stop_ <- printError(paste(checkName, Field), check_without_errors, stop_)
    }



    Field <- "DAY"
    Values <- seq(1, 31, 1)
    if (check_without_errors == TRUE) {
      if (verbose) {
        print(paste(checkName, Field, "in progress..."), quote = FALSE)
      }
      check_without_errors <- check_dictionary(ResultData = ResultDataTB, Field, Values, year = yea, wd, suffix)
    }
    if (verbose) {
      stop_ <- printError(paste(checkName, Field), check_without_errors, stop_)
    }



    Field = "HAUL_NUMBER"
    Values = seq(1,999,1)
    if (check_without_errors == TRUE) {
      if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
      check_without_errors = check_dictionary(ResultData = ResultDataTB, Field, Values, year=yea, wd, suffix)
    }
    if(verbose){stop_ = printError(paste(checkName,Field),check_without_errors, stop_)}



    Field <- "CODEND_CLOSING"
    Values <- c("S", "C")
    if (check_without_errors == TRUE) {
      if (verbose) {
        print(paste(checkName, Field, "in progress..."), quote = FALSE)
      }
      check_without_errors <- check_dictionary(ResultData = ResultDataTB, Field, Values, year = yea, wd, suffix)
    }
    if (verbose) {
      stop_ <- printError(paste(checkName, Field), check_without_errors, stop_)
    }

    Field <- "PART_OF_THE_CODEND"
    Values <- c("A", "M", "P", "S")
    if (check_without_errors == TRUE) {
      if (verbose) {
        print(paste(checkName, Field, "in progress..."), quote = FALSE)
      }
      check_without_errors <- check_dictionary(ResultData = ResultDataTB, Field, Values, year = yea, wd, suffix)
    }
    if (verbose) {
      stop_ <- printError(paste(checkName, Field), check_without_errors, stop_)
    }



    Field = "TOTAL_WEIGHT_IN_THE_HAUL"
    Values = c(0,9999999)
    if (check_without_errors == TRUE) {
      if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
      check_without_errors = check_numeric_range(Data = ResultDataTB, Field, Values, year=yea, wd, suffix)
    }
    if(verbose){stop_ = printError(paste(checkName,Field),check_without_errors, stop_)}



    Field = "TOTAL_NUMBER_IN_THE_HAUL"
    Values = c(0,9999999)
    if (check_without_errors == TRUE) {
      if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
      check_without_errors = check_numeric_range(Data = ResultDataTB, Field, Values, year=yea, wd, suffix)
    }
    if(verbose){stop_ = printError(paste(checkName,Field),check_without_errors, stop_)}



    Field = "NB_OF_FEMALES"
    Values = c(0,9999999)
    if (check_without_errors == TRUE) {
      if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
      check_without_errors = check_numeric_range(Data = ResultDataTB, Field, Values, year=yea, wd, suffix)
    }
    if(verbose){stop_ = printError(paste(checkName,Field),check_without_errors, stop_)}



    Field = "NB_OF_MALES"
    Values = c(0,9999999)
    if (check_without_errors == TRUE) {
      if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
      check_without_errors = check_numeric_range(Data = ResultDataTB, Field, Values, year=yea, wd, suffix)
    }
    if(verbose){stop_ = printError(paste(checkName,Field),check_without_errors, stop_)}



    Field = "NB_OF_UNDETERMINED"
    Values = c(0,9999999)
    if (check_without_errors == TRUE) {
      if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
      check_without_errors = check_numeric_range(Data = ResultDataTB, Field, Values, year=yea, wd, suffix)
    }
    if(verbose){stop_ = printError(paste(checkName,Field),check_without_errors, stop_)}




    # TC
    Field <- "COUNTRY"
    Values <- as.character(unique(Stratification$COUNTRY))
    if (check_without_errors == TRUE) {
      if (verbose) {
        print(paste(checkName, Field, "in progress..."), quote = FALSE)
      }
      check_without_errors <- check_dictionary(ResultData = ResultDataTC, Field, Values, year = yea, wd, suffix)
    }
    if (verbose) {
      stop_ <- printError(paste(checkName, Field), check_without_errors, stop_)
    }


    Field = "AREA"
    Values = as.character(unique(Stratification$GSA))
    if (check_without_errors == TRUE) {
      if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
      check_without_errors = check_dictionary(ResultData = ResultDataTC, Field, Values, year=yea, wd, suffix)
    }
    if(verbose){stop_ = printError(paste(checkName,Field),check_without_errors, stop_)}


    Field <- "MONTH"
    Values <- seq(1, 12, 1)
    if (check_without_errors == TRUE) {
      if (verbose) {
        print(paste(checkName, Field, "in progress..."), quote = FALSE)
      }
      check_without_errors <- check_dictionary(ResultData = ResultDataTC, Field, Values, year = yea, wd, suffix)
    }
    if (verbose) {
      stop_ <- printError(paste(checkName, Field), check_without_errors, stop_)
    }



    Field <- "DAY"
    Values <- seq(1, 31, 1)
    if (check_without_errors == TRUE) {
      if (verbose) {
        print(paste(checkName, Field, "in progress..."), quote = FALSE)
      }
      check_without_errors <- check_dictionary(ResultData = ResultDataTC, Field, Values, year = yea, wd, suffix)
    }
    if (verbose) {
      stop_ <- printError(paste(checkName, Field), check_without_errors, stop_)
    }



    Field = "HAUL_NUMBER"
    Values = seq(1,999,1)
    if (check_without_errors == TRUE) {
      if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
      check_without_errors = check_dictionary(ResultData = ResultDataTC, Field, Values, year=yea, wd, suffix)
    }
    if(verbose){stop_ = printError(paste(checkName,Field),check_without_errors, stop_)}



    Field <- "CODEND_CLOSING"
    Values <- c("S", "C")
    if (check_without_errors == TRUE) {
      if (verbose) {
        print(paste(checkName, Field, "in progress..."), quote = FALSE)
      }
      check_without_errors <- check_dictionary(ResultData = ResultDataTC, Field, Values, year = yea, wd, suffix)
    }
    if (verbose) {
      stop_ <- printError(paste(checkName, Field), check_without_errors, stop_)
    }


    Field <- "PART_OF_THE_CODEND"
    Values <- c("A", "M", "P", "S")
    if (check_without_errors == TRUE) {
      if (verbose) {
        print(paste(checkName, Field, "in progress..."), quote = FALSE)
      }
      check_without_errors <- check_dictionary(ResultData = ResultDataTC, Field, Values, year = yea, wd, suffix)
    }
    if (verbose) {
      stop_ <- printError(paste(checkName, Field), check_without_errors, stop_)
    }



    Field = "LENGTH_CLASSES_CODE"
    Values = c("m", "0", "1")
    if (check_without_errors == TRUE) {
      if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
      check_without_errors = check_dictionary(ResultData = ResultDataTC, Field, Values, year=yea, wd, suffix)
    }
    if(verbose){stop_ = printError(paste(checkName,Field),check_without_errors, stop_)}



    checkName = "Check numeric range"
    Field = "WEIGHT_OF_THE_FRACTION"
    Values = c(0,999999)
    if (check_without_errors == TRUE) {
      if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
      check_without_errors = check_numeric_range(Data = ResultDataTC, Field, Values, year=yea, wd, suffix)
    }
    if(verbose){stop_ = printError(paste(checkName,Field),check_without_errors, stop_)}



    Field = "WEIGHT_OF_THE_SAMPLE_MEASURED"
    Values = c(0,999999)
    if (check_without_errors == TRUE) {
      if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
      check_without_errors = check_numeric_range(Data = ResultDataTC, Field, Values, year=yea, wd, suffix)
    }
    if(verbose){stop_ = printError(paste(checkName,Field),check_without_errors, stop_)}



    Field = "NO_OF_INDIVIDUAL_OF_THE_ABOVE_SEX_MEASURED"
    Values = c(0,999999)
    if (check_without_errors == TRUE) {
      if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
      check_without_errors = check_numeric_range(Data = ResultDataTC, Field, Values, year=yea, wd, suffix)
    }
    if(verbose){stop_ = printError(paste(checkName,Field),check_without_errors, stop_)}



    Field = "NUMBER_OF_INDIVIDUALS_IN_THE_LENGTH_CLASS_AND_MATURITY_STAGE"
    Values = c(0,999999)
    if (check_without_errors == TRUE) {
      if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
      check_without_errors = check_numeric_range(Data = ResultDataTC, Field, Values, year=yea, wd, suffix)
    }
    if(verbose){stop_ = printError(paste(checkName,Field),check_without_errors, stop_)}




    Field <- "SEX"
    Values <- c("M", "F", "I", "N")
    if (check_without_errors == TRUE) {
      if (verbose) {
        print(paste(checkName, Field, "in progress..."), quote = FALSE)
      }
      check_without_errors <- check_dictionary(ResultData = ResultDataTC, Field, Values, year = yea, wd, suffix)
    }
    if (verbose) {
      stop_ <- printError(paste(checkName, Field), check_without_errors, stop_)
    }



    Field = "LENGTH_CLASS"
    Values = c(1:9999)
    if (check_without_errors == TRUE) {
      if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
      check_without_errors = check_dictionary(ResultData = ResultDataTC, Field, Values, year=yea, wd, suffix)
    }
    if(verbose){stop_ = printError(paste(checkName,Field),check_without_errors, stop_)}



    # TE

    if (!(all(is.na(TE)) & length(TE) == 1)) {
      if (nrow(ResultDataTE) > 0) {
        Field <- "COUNTRY"
        Values <- as.character(unique(Stratification$COUNTRY))
        if (check_without_errors == TRUE) {
          if (verbose) {
            print(paste(checkName, Field, "in progress..."), quote = FALSE)
          }
          check_without_errors <- check_dictionary(ResultData = ResultDataTE, Field, Values, year = yea, wd, suffix)
        }
        if (verbose) {
          stop_ <- printError(paste(checkName, Field), check_without_errors, stop_)
        }



        Field = "AREA"
        Values = as.character(unique(Stratification$GSA))
        if (check_without_errors == TRUE) {
          if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
          check_without_errors = check_dictionary(ResultData = ResultDataTE, Field, Values, year=yea, wd, suffix)
        }
        if(verbose){stop_ = printError(paste(checkName,Field),check_without_errors, stop_)}



        Field <- "MONTH"
        Values <- seq(1, 12, 1)
        if (check_without_errors == TRUE) {
          if (verbose) {
            print(paste(checkName, Field, "in progress..."), quote = FALSE)
          }
          check_without_errors <- check_dictionary(ResultData = ResultDataTE, Field, Values, year = yea, wd, suffix)
        }
        if (verbose) {
          stop_ <- printError(paste(checkName, Field), check_without_errors, stop_)
        }



        Field <- "DAY"
        Values <- seq(1, 31, 1)
        if (check_without_errors == TRUE) {
          if (verbose) {
            print(paste(checkName, Field, "in progress..."), quote = FALSE)
          }
          check_without_errors <- check_dictionary(ResultData = ResultDataTE, Field, Values, year = yea, wd, suffix)
        }
        if (verbose) {
          stop_ <- printError(paste(checkName, Field), check_without_errors, stop_)
        }



        Field = "HAUL_NUMBER"
        Values = seq(1,999,1)
        if (check_without_errors == TRUE) {
          if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
          check_without_errors = check_dictionary(ResultData = ResultDataTE, Field, Values, year=yea, wd, suffix)
        }
        if(verbose){stop_ = printError(paste(checkName,Field),check_without_errors, stop_)}



        Field <- "LENGTH_CLASSES_CODE"
        Values <- c("0", "m")
        if (check_without_errors == TRUE) {
          if (verbose) {
            print(paste(checkName, Field, "in progress..."), quote = FALSE)
          }
          check_without_errors <- check_dictionary(ResultData = ResultDataTE, Field, Values, year = yea, wd, suffix)
        }
        if (verbose) {
          stop_ <- printError(paste(checkName, Field), check_without_errors, stop_)
        }

        # ResultDataTE[is.na(ResultDataTE$LENGTH_CLASSES_CODE),]

        Field <- "SEX"
        Values <- c("M", "F", "I", "N")
        if (check_without_errors == TRUE) {
          if (verbose) {
            print(paste(checkName, Field, "in progress..."), quote = FALSE)
          }
          check_without_errors <- check_dictionary(ResultData = ResultDataTE, Field, Values, year = yea, wd, suffix)
        }
        if (verbose) {
          stop_ <- printError(paste(checkName, Field), check_without_errors, stop_)
        }



        Field = "NO_PER_SEX_MEASURED_IN_SUB_SAMPLE_FOR_OTOLITH"
        Values = c(0,999999)
        if (check_without_errors == TRUE) {
          if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
          check_without_errors = check_numeric_range(Data = ResultDataTE, Field, Values, year=yea, wd, suffix)
        }
        if(verbose){stop_ = printError(paste(checkName,Field),check_without_errors, stop_)}



        Field = "NO_PER_SEX_MEASURED_IN_SUB_SAMPLE_FOR_WEIGHT"
        Values = c(1,999999)
        if (check_without_errors == TRUE) {
          if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
          check_without_errors = check_numeric_range(Data = ResultDataTE, Field, Values, year=yea, wd, suffix)
        }
        if(verbose){stop_ = printError(paste(checkName,Field),check_without_errors, stop_)}



        Field = "NO_PER_SEX_MEASURED_IN_SUB_SAMPLE_FOR_AGEING"
        Values = c(0,999999)
        if (check_without_errors == TRUE) {
          if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
          check_without_errors = check_numeric_range(Data = ResultDataTE, Field, Values, year=yea, wd, suffix)
        }
        if(verbose){stop_ = printError(paste(checkName,Field),check_without_errors, stop_)}



        Field = "RECORD_NUMBER"
        Values = c(0,100000)
        if (check_without_errors == TRUE) {
          if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
          check_without_errors = check_numeric_range(Data = ResultDataTE, Field, Values, year=yea, wd, suffix)
        }
        if(verbose){stop_ = printError(paste(checkName,Field),check_without_errors, stop_)}



        Field = "LENGTH_CLASS"
        Values = c(1:9999)
        if (check_without_errors == TRUE) {
          if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
          check_without_errors = check_dictionary(ResultData = ResultDataTE, Field, Values, year=yea, wd, suffix)
        }
        if(verbose){stop_ = printError(paste(checkName,Field),check_without_errors, stop_)}



        Field = "MATURITY"
        Values = c(0:4)
        if (check_without_errors == TRUE) {
          if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
          check_without_errors = check_dictionary(ResultData = ResultDataTE, Field, Values, year=yea, wd, suffix)
        }
        if(verbose){stop_ = printError(paste(checkName,Field),check_without_errors, stop_)}


        Field = "MATSUB"
        Values = c("A","B","C","D","E","O")
        if (check_without_errors == TRUE) {
          if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
          check_without_errors = check_dictionary(ResultData = ResultDataTE, Field, Values, year=yea, wd, suffix)
        }
        if(verbose){stop_ = printError(paste(checkName,Field),check_without_errors, stop_)}



        Field = "OTOLITH_SAMPLED"
        Values = c("Y","N","NR")
        if (check_without_errors == TRUE) {
          if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
          check_without_errors = check_dictionary(ResultData = ResultDataTE, Field, Values, year=yea, wd, suffix)
        }
        if(verbose){stop_ = printError(paste(checkName,Field),check_without_errors, stop_)}



        Field = "OTOLITH_READ"
        Values = c("Y","N","NR")
        if (check_without_errors == TRUE) {
          if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
          check_without_errors = check_dictionary(ResultData = ResultDataTE, Field, Values, year=yea, wd, suffix)
        }
        if(verbose){stop_ = printError(paste(checkName,Field),check_without_errors, stop_)}


        Field = "AGE"
        Values = c(seq(-1,99,0.1),"UR","NR")
        if (check_without_errors == TRUE) {
          if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
          check_without_errors = check_dictionary(ResultData = ResultDataTE, Field, Values, year=yea, wd, suffix)
        }
        if(verbose){stop_ = printError(paste(checkName,Field),check_without_errors, stop_)}


      }
    }

    # TL
    if (!(all(is.na(TL)) & length(TL) == 1)) {
      if (nrow(ResultDataTL) > 0) {
        Field <- "COUNTRY"
        Values <- as.character(unique(Stratification$COUNTRY))
        if (check_without_errors == TRUE) {
          if (verbose) {
            print(paste(checkName, Field, "in progress..."), quote = FALSE)
          }
          check_without_errors <- check_dictionary(ResultData = ResultDataTL, Field, Values, year = yea, wd, suffix)
        }
        if (verbose) {
          stop_ <- printError(paste(checkName, Field), check_without_errors, stop_)
        }



        Field = "AREA"
        Values = as.character(unique(Stratification$GSA))
        if (check_without_errors == TRUE) {
          if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
          check_without_errors = check_dictionary(ResultData = ResultDataTL, Field, Values, year=yea, wd, suffix)
        }
        if(verbose){stop_ = printError(paste(checkName,Field),check_without_errors, stop_)}



        Field <- "MONTH"
        Values <- seq(1, 12, 1)
        if (check_without_errors == TRUE) {
          if (verbose) {
            print(paste(checkName, Field, "in progress..."), quote = FALSE)
          }
          check_without_errors <- check_dictionary(ResultData = ResultDataTL, Field, Values, year = yea, wd, suffix)
        }
        if (verbose) {
          stop_ <- printError(paste(checkName, Field), check_without_errors, stop_)
        }


        Field <- "DAY"
        Values <- seq(1, 31, 1)
        if (check_without_errors == TRUE) {
          if (verbose) {
            print(paste(checkName, Field, "in progress..."), quote = FALSE)
          }
          check_without_errors <- check_dictionary(ResultData = ResultDataTL, Field, Values, year = yea, wd, suffix)
        }
        if (verbose) {
          stop_ <- printError(paste(checkName, Field), check_without_errors, stop_)
        }


        Field = "HAUL_NUMBER"
        Values = seq(1,999,1)
        if (check_without_errors == TRUE) {
          if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
          check_without_errors = check_dictionary(ResultData = ResultDataTL, Field, Values, year=yea, wd, suffix)
        }
        if(verbose){stop_ = printError(paste(checkName,Field),check_without_errors, stop_)}



        checkName = "Check numeric range"
        Field = "TOTAL_WEIGHT_IN_THE_CATEGORY_HAUL"
        Values = c(0,9999999)
        if (check_without_errors == TRUE) {
          if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
          check_without_errors = check_numeric_range(Data = ResultDataTL, Field, Values, year=yea, wd, suffix)
        }
        if(verbose){stop_ = printError(paste(checkName,Field),check_without_errors, stop_)}


        checkName = "Check numeric range"
        Field = "TOTAL_NUMBER_IN_THE_CATEGORY_HAUL"
        Values = c(0,9999999)
        if (check_without_errors == TRUE) {
          if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
          check_without_errors = check_numeric_range(Data = ResultDataTL, Field, Values, year=yea, wd, suffix)
        }
        if(verbose){stop_ = printError(paste(checkName,Field),check_without_errors, stop_)}



        checkName = "Check numeric range"
        Field = "TOTAL_WEIGHT_IN_THE_SUB-CATEGORY_HAUL"
        Values = c(0,9999999)
        if (check_without_errors == TRUE) {
          if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
          check_without_errors = check_numeric_range(Data = ResultDataTL, Field, Values, year=yea, wd, suffix)
        }
        if(verbose){stop_ = printError(paste(checkName,Field),check_without_errors, stop_)}



        checkName = "Check numeric range"
        Field = "TOTAL_NUMBER_IN_THE_SUB-CATEGORY_HAUL"
        Values = c(0,9999999)
        if (check_without_errors == TRUE) {
          if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
          check_without_errors = check_numeric_range(Data = ResultDataTL, Field, Values, year=yea, wd, suffix)
        }
        if(verbose){stop_ = printError(paste(checkName,Field),check_without_errors, stop_)}

      }
    }

    # End dictionary checks
    #-------------------------------------

    #-------------------------------------
    # Check no empty fields

    checkName <- "Check no empty fields TA"
    if (check_without_errors == TRUE) {
      if (verbose) {
        print(paste(checkName, "in progress..."), quote = FALSE)
      }
      check_without_errors <- check_no_empty_fields(ResultDataTA, year = yea, wd, suffix)
    }
    if (verbose) {
      stop_ <- printError(checkName, check_without_errors, stop_)
    }

    checkName <- "Check no empty fields TB"
    if (check_without_errors == TRUE) {
      if (verbose) {
        print(paste(checkName, "in progress..."), quote = FALSE)
      }
      check_without_errors <- check_no_empty_fields(ResultDataTB, year = yea, wd, suffix)
    }
    if (verbose) {
      stop_ <- printError(checkName, check_without_errors, stop_)
    }

    checkName <- "Check no empty fields TC"
    if (check_without_errors == TRUE) {
      if (verbose) {
        print(paste(checkName, "in progress..."), quote = FALSE)
      }
      check_without_errors <- check_no_empty_fields(ResultDataTC, year = yea, wd, suffix)
    }
    if (verbose) {
      stop_ <- printError(checkName, check_without_errors, stop_)
    }


    if (!(all(is.na(TE)) & length(TE) == 1)) {
      if (nrow(ResultDataTE) > 0) {
        checkName <- "Check no empty fields TE"
        if (check_without_errors == TRUE) {
          if (verbose) {
            print(paste(checkName, "in progress..."), quote = FALSE)
          }
          check_without_errors <- check_no_empty_fields(ResultDataTE, year = yea, wd, suffix)
        }
        if (verbose) {
          stop_ <- printError(checkName, check_without_errors, stop_)
        }
      }
    }

    if (!(all(is.na(TL)) & length(TL) == 1)) {
      if (nrow(ResultDataTL) > 0) {
        checkName <- "Check no empty fields TL"
        if (check_without_errors == TRUE) {
          if (verbose) {
            print(paste(checkName, "in progress..."), quote = FALSE)
          }
          check_without_errors <- check_no_empty_fields(ResultDataTL, year = yea, wd, suffix)
        }
        if (verbose) {
          stop_ <- printError(checkName, check_without_errors, stop_)
        }
      }
    }

    #-------------------------------------
    # checks on TA

    checkName <- "Check 0 fields TA"
    if (check_without_errors == TRUE) {
      if (verbose) {
        print(paste(checkName, "in progress..."), quote = FALSE)
      }
      check_without_errors <- check_0_fieldsTA(ResultDataTA, wd, suffix, year = yea)
    }
    if (verbose) {
      stop_ <- printError(checkName, check_without_errors, stop_)
    }

    checkName <- "Check dm TA"
    if (check_without_errors == TRUE) {
      if (verbose) {
        print(paste(checkName, "in progress..."), quote = FALSE)
      }
      check_without_errors <- check_dm(ResultDataTA, year = yea, wd, suffix)
    }
    if (verbose) {
      stop_ <- printError(checkName, check_without_errors, stop_)
    }

    checkName <- "Check consistency between duration and time TA"
    if (check_without_errors == TRUE) {
      if (verbose) {
        print(paste(checkName, "in progress..."), quote = FALSE)
      }
      check_without_errors <- check_consistencyTA_duration(ResultDataTA, year = yea, wd, suffix)
    }
    if (verbose) {
      stop_ <- printError(checkName, check_without_errors, stop_)
    }

    checkName <- "Check consistency between distance and duration of the haul TA"
    if (check_without_errors == TRUE) {
      if (verbose) {
        print(paste(checkName, "in progress..."), quote = FALSE)
      }
      check_without_errors <- check_consistencyTA_distance(ResultDataTA, year = yea, wd, suffix)
    }
    if (verbose) {
      stop_ <- printError(checkName, check_without_errors, stop_)
    }

    checkName <- "Check consistency of the hauls coordinates with the distance (difference not greater than 30%)"
    if (check_without_errors == TRUE) {
      if (verbose) {
        print(paste(checkName, "in progress..."), quote = FALSE)
      }
      check_without_errors <- check_distance(ResultDataTA, year = yea, wd, suffix)
    }
    if (verbose) {
      stop_ <- printError(checkName, check_without_errors, stop_)
    }

    checkName <- "Check consistency of bridles length TA"
    if (check_without_errors == TRUE) {
      if (verbose) {
        print(paste(checkName, "in progress..."), quote = FALSE)
      }
      check_without_errors <- check_bridles_length(ResultDataTA, year = yea, wd, suffix)
    }
    if (verbose) {
      stop_ <- printError(checkName, check_without_errors, stop_)
    }

    checkName <- "Check difference between start depth and end depth (not greater than 20%) in TA"
    if (check_without_errors == TRUE) {
      if (verbose) {
        print(paste(checkName, "in progress..."), quote = FALSE)
      }
      check_without_errors <- check_depth(ResultDataTA, year = yea, wd, suffix)
    }
    if (verbose) {
      stop_ <- printError(checkName, check_without_errors, stop_)
    }

    checkName <- "Check start depth and end depth in the same stratum TA"
    if (check_without_errors == TRUE) {
      if (verbose) {
        print(paste(checkName, "in progress..."), quote = FALSE)
      }
      check_without_errors <- check_stratum(ResultDataTA, year = yea, wd, suffix)
    }
    if (verbose) {
      stop_ <- printError(checkName, check_without_errors, stop_)
    }

    checkName <- "Check consistency of stratum code in TA"
    if (check_without_errors == TRUE) {
      if (verbose) {
        print(paste(checkName, "in progress..."), quote = FALSE)
      }
      check_without_errors <- check_stratum_code(ResultDataTA, year = yea, Strata = Stratification, wd, suffix)
    }
    if (verbose) {
      stop_ <- printError(checkName, check_without_errors, stop_)
    }

    checkName <- "Check start quadrant and end quadrant TA"
    if (check_without_errors == TRUE) {
      if (verbose) {
        print(paste(checkName, "in progress..."), quote = FALSE)
      }
      check_without_errors <- check_quadrant(ResultDataTA, year = yea, wd, suffix)
    }
    if (verbose) {
      stop_ <- printError(checkName, check_without_errors, stop_)
    }

    checkName <- "Check uniqueness of valid hauls TA"
    if (check_without_errors == TRUE) {
      if (verbose) {
        print(paste(checkName, "in progress..."), quote = FALSE)
      }
      check_without_errors <- check_unique_valid_haul(ResultDataTA, year = yea, wd, suffix)
    }
    if (verbose) {
      stop_ <- printError(checkName, check_without_errors, stop_)
    }

    checkName <- "Visual check of the haul positions"
    if (check_without_errors == TRUE) {
      if (verbose) {
        print(paste(checkName, "in progress..."), quote = FALSE)
      }
      check_position(ResultDataTA, year = yea, wd, suffix)
    }
    if (verbose) {
      stop_ <- printError(checkName, check_without_errors, stop_)
    }

    checkName <- "Relation between shooting depth and warp length, and between warp length and wing opening"
    if (check_without_errors == TRUE) {
      if (verbose) {
        print(paste(checkName, "in progress..."), quote = FALSE)
      }
      graphs_TA(ResultDataTA, year = yea, wd, suffix)
    }
    if (verbose) {
      stop_ <- printError(checkName, check_without_errors, stop_)
    }

    checkName <- "Check if the coordinates are in the Mediterranean Sea"
    if (check_without_errors == TRUE) {
      if (verbose) {
        print(paste(checkName, "in progress..."), quote = FALSE)
      }
      check_without_errors <- check_position_in_Med(ResultDataTA, year = yea, wd, suffix)
    }
    if (verbose) {
      stop_ <- printError(checkName, check_without_errors, stop_)
    }

    checkName <- "Check on temperature by haul"
    if (check_without_errors == TRUE) {
      if (verbose) {
        print(paste(checkName, "in progress..."), quote = FALSE)
      }
      check_without_errors <- check_temperature(ResultDataTA, year = yea, wd, suffix)
    }
    if (verbose) {
      stop_ <- printError(checkName, check_without_errors, stop_)
    }



    # checks on TB

    checkName <- "Check correctness of species codes TB"
    if (check_without_errors == TRUE) {
      if (verbose) {
        print(paste(checkName, "in progress..."), quote = FALSE)
      }
      check_without_errors <- check_rubincode(ResultData=ResultDataTB, year = yea, TMlist = Ref_list, wd, suffix)
    }
    if (verbose) {
      stop_ <- printError(checkName, check_without_errors, stop_)
    }

    checkName <- "Check consistency of NB_TOTAL and number per sex TB"
    if (check_without_errors == TRUE) {
      if (verbose) {
        print(paste(checkName, "in progress..."), quote = FALSE)
      }
      check_without_errors <- check_nbtotTB(ResultDataTB, year = yea, wd, suffix)
    }
    if (verbose) {
      stop_ <- printError(checkName, check_without_errors, stop_)
    }

    checkName <- "Check presence of NB_TOTAL and number per sex TB for species G1"
    if (check_without_errors == TRUE) {
      if (verbose) {
        print(paste(checkName, "in progress..."), quote = FALSE)
      }
      check_without_errors <- check_nm_TB(ResultDataTB, year = yea, wd, suffix)
    }
    if (verbose) {
      stop_ <- printError(checkName, check_without_errors, stop_)
    }

    checkName <- "Check consistency between not null weight and not null total number"
    if (check_without_errors == TRUE) {
      if (verbose) {
        print(paste(checkName, "in progress..."), quote = FALSE)
      }
      check_without_errors <- check_weight_tot_nb(ResultDataTB, year = yea, wd, suffix)
    }
    if (verbose) {
      stop_ <- printError(checkName, check_without_errors, stop_)
    }

    checkName <- "Check consistency of weight and number TB"
    if (check_without_errors == TRUE) {
      if (verbose) {
        print(paste(checkName, "in progress..."), quote = FALSE)
      }
      check_without_errors <- check_weight(ResultDataTB, year = yea, DataTargetSpecies = RoME::DataTargetSpecies, wd, suffix)
    }
    if (verbose) {
      stop_ <- printError(checkName, check_without_errors, stop_)
    }

    # checks on TC

    graphics.off()

    checkName <- "Check correctness of species codes TC"
    if (check_without_errors == TRUE) {
      if (verbose) {
        print(paste(checkName, "in progress..."), quote = FALSE)
      }
      check_without_errors <- check_rubincode(ResultDataTC, year = yea, TMlist = Ref_list, wd, suffix)
    }
    if (verbose) {
      stop_ <- printError(checkName, check_without_errors, stop_)
    }


    checkName <- "Check correctness of LENGTH_CLASSES_CODE TC"
    if (check_without_errors == TRUE) {
      if (verbose) {
        print(paste(checkName, "in progress..."), quote = FALSE)
      }
      check_without_errors <- check_length_class_codeTC(ResultDataTC, Specieslist = Ref_list, year = yea, wd, suffix)
    }
    if (verbose) {
      stop_ <- printError(checkName, check_without_errors, stop_)
    }

    checkName <- "Check consistency of length classes TC"
    if (check_without_errors == TRUE) {
      if (verbose) {
        print(paste(checkName, "in progress..."), quote = FALSE)
      }
      check_without_errors <- check_length(ResultDataTC, DataSpecies = NA, year = yea, wd, suffix)
    }
    if (verbose) {
      stop_ <- printError(checkName, check_without_errors, stop_)
    }

    checkName <- "Check correctness of number per sex in TC"
    if (check_without_errors == TRUE) {
      if (verbose) {
        print(paste(checkName, "in progress..."), quote = FALSE)
      }
      check_without_errors <- check_nb_per_sexTC(ResultDataTC, year = yea, wd, suffix)
    }
    if (verbose) {
      stop_ <- printError(checkName, check_without_errors, stop_)
    }

    checkName <- "Check consistency of maturity stages TC"
    if (check_without_errors == TRUE) {
      if (verbose) {
        print(paste(checkName, "in progress..."), quote = FALSE)
      }
      check_without_errors <- check_mat_stages(ResultDataTC, year = yea, wd, suffix, stages = stages_list)
    }
    if (verbose) {
      stop_ <- printError(checkName, check_without_errors, stop_)
    }

    checkName <- "Check sub-sampling"
    if (check_without_errors == TRUE) {
      if (verbose) {
        print(paste(checkName, "in progress..."), quote = FALSE)
      }
      check_without_errors <- check_subsampling(ResultDataTC, year = yea, wd, suffix)
    }
    if (verbose) {
      stop_ <- printError(checkName, check_without_errors, stop_)
    }

    checkName <- "Check consistency of maturity stages TC by the comparison with the length of smallest mature individuals reported in bibliography"
    if (check_without_errors == TRUE) {
      if (verbose) {
        print(paste(checkName, "in progress..."), quote = FALSE)
      }
      check_without_errors <- check_smallest_mature(ResultDataTC, year = yea, MaturityParameters = Maturity, TargetSpecies = DataTargetSpecies, wd, suffix)
    }
    if (verbose) {
      stop_ <- printError(checkName, check_without_errors, stop_)
    }

    checkName <- "Check consistency of sex TC by means of spawning period"
    if (check_without_errors == TRUE) {
      if (verbose) {
        print(paste(checkName, "in progress..."), quote = FALSE)
      }
      check_without_errors <- check_spawning_period(ResultDataTA, ResultDataTC, year = yea, Maturity_parameters = Maturity, DataTargetSpecies = DataTargetSpecies, wd, suffix)
    }
    if (verbose) {
      stop_ <- printError(checkName, check_without_errors, stop_)
    }

    # checkName = "Check consistency of sex data TC by means of sex-inversion size"
    # if (check_without_errors == TRUE) {
    #   if(verbose){print(paste(checkName,"in progress..."), quote = FALSE)}
    #   check_without_errors = check_sex_inversion(ResultDataTC,Maturity,wd,suffix)
    # }
    #   if(verbose){stop_ = printError(checkName,check_without_errors, stop_)}

    checkName <- "Check consistency of length distribution TC"
    if (check_without_errors == TRUE) {
      if (verbose) {
        print(paste(checkName, "in progress..."), quote = FALSE)
      }
      check_without_errors <- check_step_length_distr(ResultDataTC, year = yea, wd, suffix)
    }
    if (verbose) {
      stop_ <- printError(checkName, check_without_errors, stop_)
    }

    checkName <- "Check total weight in the haul in TC"
    if (check_without_errors == TRUE) {
      if (verbose) {
        print(paste(checkName, "in progress..."), quote = FALSE)
      }
      check_without_errors <- check_individual_weightTC(ResultDataTC, LW = ab_parameters, year = yea, wd, suffix, verbose = FALSE)
    }
    if (verbose) {
      stop_ <- printError(checkName, check_without_errors, stop_)
    }

    checkName <- "Check correctness of species codes TC"
    if (check_without_errors == TRUE) {
      if (verbose) {
        print(paste(checkName, "in progress..."), quote = FALSE)
      }
      check_without_errors <- check_rubincode(ResultDataTC, year = yea, TMlist = Ref_list, wd, suffix)
    }
    if (verbose) {
      stop_ <- printError(checkName, check_without_errors, stop_)
    }

    checkName <- "Check presence of lengths for G1 and G2 Medits species in TC"
    if (check_without_errors == TRUE) {
      if (verbose) {
        print(paste(checkName, "in progress..."), quote = FALSE)
      }
      check_without_errors <- check_G1_G2(ResultDataTC, year = yea, wd, suffix)
    }
    if (verbose) {
      stop_ <- printError(checkName, check_without_errors, stop_)
    }

    # cross checks

    checkName <- "Check presence in TB of TA hauls"
    if (check_without_errors == TRUE) {
      if (verbose) {
        print(paste(checkName, "in progress..."), quote = FALSE)
      }
      check_without_errors <- check_hauls_TATB(ResultDataTA, ResultDataTB, year = yea, wd, suffix)
    }
    if (verbose) {
      stop_ <- printError(checkName, check_without_errors, stop_)
    }

    checkName <- "Check presence in TA of TB hauls"
    if (check_without_errors == TRUE) {
      if (verbose) {
        print(paste(checkName, "in progress..."), quote = FALSE)
      }
      check_without_errors <- check_hauls_TBTA(ResultDataTA, ResultDataTB, year = yea, wd, suffix)
    }
    if (verbose) {
      stop_ <- printError(checkName, check_without_errors, stop_)
    }

    checkName <- "Check presence in TC of TB target species"
    if (check_without_errors == TRUE) {
      if (verbose) {
        print(paste(checkName, "in progress..."), quote = FALSE)
      }
      check_without_errors <- check_species_TBTC(ResultDataTB, ResultDataTC, year = yea, DataSpecies = DataTargetSpecies, wd, suffix)
    }
    if (verbose) {
      stop_ <- printError(checkName, check_without_errors, stop_)
    }

    checkName <- "Check presence in TB of TC species"
    if (check_without_errors == TRUE) {
      if (verbose) {
        print(paste(checkName, "in progress..."), quote = FALSE)
      }
      check_without_errors <- check_haul_species_TCTB(ResultDataTB, ResultDataTC, year = yea, wd, suffix)
    }
    if (verbose) {
      stop_ <- printError(checkName, check_without_errors, stop_)
    }

    checkName <- "Check correctness of the number per sex in TB in case of sub-sampling in TC  "
    if (check_without_errors == TRUE) {
      if (verbose) {
        print(paste(checkName, "in progress..."), quote = FALSE)
      }
      check_without_errors <- check_raising(ResultDataTB, ResultDataTC, year = yea, wd, suffix)
    }
    if (verbose) {
      stop_ <- printError(checkName, check_without_errors, stop_)
    }


    checkName <- "Check on date by haul TB"
    if (check_without_errors == TRUE) {
      if (verbose) {
        print(paste(checkName, "in progress..."), quote = FALSE)
      }
      check_without_errors <- check_date_haul(ResultDataTA, ResultDataTB, year = yea, wd, suffix)
    }
    if (verbose) {
      stop_ <- printError(checkName, check_without_errors, stop_)
    }

    checkName <- "Check on date by haul TC"
    if (check_without_errors == TRUE) {
      if (verbose) {
        print(paste(checkName, "in progress..."), quote = FALSE)
      }
      check_without_errors <- check_date_haul(ResultDataTA, ResultDataTC, year = yea, wd, suffix)
    }
    if (verbose) {
      stop_ <- printError(checkName, check_without_errors, stop_)
    }


    # Check on TE

    if (!(all(is.na(TE)) & length(TE) == 1)) {
      if (nrow(ResultDataTE) > 0) {
        checkName <- "Check on date by haul TE"
        if (check_without_errors == TRUE) {
          if (verbose) {
            print(paste(checkName, "in progress..."), quote = FALSE)
          }
          check_without_errors <- check_date_haul(ResultDataTA, ResultDataTE, year = yea, wd, suffix)
        }
        if (verbose) {
          stop_ <- printError(checkName, check_without_errors, stop_)
        }



        checkName <- "Check consistency of maturity stages TE"
        if (check_without_errors == TRUE) {
          if (verbose) {
            print(paste(checkName, "in progress..."), quote = FALSE)
          }
          check_without_errors <- check_mat_stages(ResultDataTE, year = yea, wd, suffix, stages = stages_list)
        }
        if (verbose) {
          stop_ <- printError(checkName, check_without_errors, stop_)
        }



        checkName <- "Cross check on number between TC and TE"
        if (check_without_errors == TRUE) {
          if (verbose) {
            print(paste(checkName, "in progress..."), quote = FALSE)
          }
          check_without_errors <- check_TE_TC(ResultDataTC, ResultDataTE, year = yea, wd, suffix)
        }
        if (verbose) {
          stop_ <- printError(checkName, check_without_errors, stop_)
        }


        checkName <- "Check consistency of maturity stages TE by the comparison with the length of smallest mature individuals reported in bibliography"
        if (check_without_errors == TRUE) {
          if (verbose) {
            print(paste(checkName, "in progress..."), quote = FALSE)
          }
          check_without_errors <- check_smallest_mature(ResultDataTE, year = yea, MaturityParameters = Maturity, TargetSpecies = DataTargetSpecies, wd, suffix)
        }
        if (verbose) {
          stop_ <- printError(checkName, check_without_errors, stop_)
        }


        checkName <- "Check consistency of maturity stages in TE by means of spawning period"
        if (check_without_errors == TRUE) {
          if (verbose) {
            print(paste(checkName, "in progress..."), quote = FALSE)
          }
          check_without_errors <- check_spawning_period(ResultDataTA, ResultDataTE, year = yea, Maturity_parameters = Maturity, DataTargetSpecies = DataTargetSpecies, wd, suffix)
        }
        if (verbose) {
          stop_ <- printError(checkName, check_without_errors, stop_)
        }


        checkName <- "Check individual weight in TE"
        if (check_without_errors == TRUE) {
          if (verbose) {
            print(paste(checkName, "in progress..."), quote = FALSE)
          }
          check_without_errors <- check_individual_weightTE(ResultDataTE, LW = ab_parameters, year = yea, wd, suffix, verbose = FALSE)
        }
        if (verbose) {
          stop_ <- printError(checkName, check_without_errors, stop_)
        }

        checkName <- "Check correctness of species codes TE"
        if (check_without_errors == TRUE) {
          if (verbose) {
            print(paste(checkName, "in progress..."), quote = FALSE)
          }
          check_without_errors <- check_rubincode(ResultDataTE, year = yea, TMlist = Ref_list, wd, suffix)
        }
        if (verbose) {
          stop_ <- printError(checkName, check_without_errors, stop_)
        }

        checkName <- "Check consistency TE check-fields"
        if (check_without_errors == TRUE) {
          if (verbose) {
            print(paste(checkName, "in progress..."), quote = FALSE)
          }
          check_without_errors <- check_nb_TE(ResultDataTE, year = yea, wd, suffix)
        }
        if (verbose) {
          stop_ <- printError(checkName, check_without_errors, stop_)
        }

        checkName <- "Summary individual data sampling"
        if (check_without_errors == TRUE) {
          if (verbose) {
            print(paste(checkName, "in progress..."), quote = FALSE)
          }
          check_without_errors <- scheme_individual_data(DataTC = ResultDataTC, DataTE = ResultDataTE, year = yea, wd, suffix)
        }
        if (verbose) {
          stop_ <- printError(checkName, check_without_errors, stop_)
        }
      }
    }



    # Checks on TL
    if (!(all(is.na(TL)) & length(TL) == 1)) {
      if (nrow(ResultDataTL) > 0) {
        checkName <- "Check allowed values for category on Litter data"
        Field <- "LITTER_CATEGORY"
        Values <- c("L0", "L1", "L2", "L3", "L4", "L5", "L6", "L7", "L8", "L9")
        if (check_without_errors == TRUE) {
          if (verbose) {
            print(paste(checkName, "in progress..."), quote = FALSE)
          }
          check_without_errors <- check_dictionary(ResultData = ResultDataTL, Field, Values, year = yea, wd, suffix)
        }
        if (verbose) {
          stop_ <- printError(checkName, check_without_errors, stop_)
        }

        checkName <- "Check allowed values for sub-category on Litter data"
        Field <- "LITTER_SUB-CATEGORY"
        Values <- c("0", "A", "B", "C", "D", "E", "F", "G", "H", "I", "J","a","b","c","d","e","f","g","h","i","j")
        colnames(ResultDataTL)[10] <- "LITTER_SUB-CATEGORY"

        if (check_without_errors == TRUE) {
          if (verbose) {
            print(paste(checkName, "in progress..."), quote = FALSE)
          }
          check_without_errors <- check_dictionary(ResultData = ResultDataTL, Field, Values, year = yea, wd, suffix)
        }
        if (verbose) {
          stop_ <- printError(checkName, check_without_errors, stop_)
        }

        checkName <- "Check correctness of associations between category and sub-category on Litter data"
        if (check_without_errors == TRUE) {
          if (verbose) {
            print(paste(checkName, "in progress..."), quote = FALSE)
          }
          check_without_errors <- check_associations_category_TL(ResultDataTL, assTL, year = yea, wd, suffix)
        }
        if (verbose) {
          stop_ <- printError(checkName, check_without_errors, stop_)
        }

        # checkName = "Check if the number is always filled in on Litter data"
        # if (check_without_errors == TRUE) {
        #       if(verbose){print(paste(checkName,"in progress..."), quote = FALSE)}
        #       check_without_errors = check_no_empty_fields(ResultDataTL,year=yea,wd,suffix)
        #     }
        #     if(verbose){stop_ = printError(checkName,check_without_errors, stop_)}

        checkName <- "Check if the hauls in TL are present in TA"
        if (check_without_errors == TRUE) {
          if (verbose) {
            print(paste(checkName, "in progress..."), quote = FALSE)
          }
          check_without_errors <- check_hauls_TLTA(ResultDataTA, ResultDataTL, year = yea, wd, suffix)
        }
        if (verbose) {
          stop_ <- printError(checkName, check_without_errors, stop_)
        }

        checkName <- "Check if the hauls in TA are present in TL"
        if (check_without_errors == TRUE) {
          if (verbose) {
            print(paste(checkName, "in progress..."), quote = FALSE)
          }
          check_without_errors <- check_hauls_TATL(ResultDataTA, ResultDataTL, year = yea, wd, suffix)
        }
        if (verbose) {
          stop_ <- printError(checkName, check_without_errors, stop_)
        }

        checkName <- "Check if the date in TL is consistent with TA"
        if (check_without_errors == TRUE) {
          if (verbose) {
            print(paste(checkName, "in progress..."), quote = FALSE)
          }
          check_without_errors <- check_date_haul(ResultDataTA, ResultDataTL, year = yea, wd, suffix)
        }
        if (verbose) {
          stop_ <- printError(checkName, check_without_errors, stop_)
        }
      }
    }


    if (!stop_) {
      if (verbose) {
        print("All the checks have been performed!", quote = FALSE)
      }
    }



    # Create files for R_Sufi

    if ((!stop_) & (create_RSufi_files == TRUE)) {
      AREA <- ResultDataTA[1, "AREA"]
      create_strata(Stratification = Stratification, AREA, wd, save = TRUE)
      create_haul(ResultDataTA, year = yea, wd, save = TRUE)
      create_catch(ResultDataTB, year = yea, wd, save = TRUE)


      # if (!(all(is.na(TE)) & length(TE)==1)) {
      #   if (nrow(ResultDataTE)>0){
      #       create_length(ResultDataTE,DataSpecies=Ref_list,wd)
      #   }
      # }

      if (nrow(ResultDataTC) > 0) {
        create_length(ResultDataTC, year = yea, DataSpecies = Ref_list, wd, save = TRUE)
      }

      print(paste("R-Sufi files have been created for the ", yea, "and the GSA selected! They have been stored in files R-Sufi directory."), quote = FALSE)
    }
  } # ciclo years


  # END ---------------------------------------------------------------------

  # -------------------------------------------------------
  # -------------------------------------------------------
  # -------------------------------------------------------
  # -------------------------------------------------------


  if ((!stop_) & (create_global_RSufi_files == TRUE)) {
    if (is.na(Year_start) | is.na(Year_end)) {
      warning("One or both variables Year_start and Year_end not declared")
    } else {
      RSufi_files(Year_start, Year_end, AREA, wd)
      print("R-Sufi files have been created for the Years and the GSA selected! They have been stored in files R-Sufi directory.", quote = FALSE)
    }
  }
  # -------------------------------------------------------
  if (file.exists(file.path(tempdir(), "Logfiles"))) {
    unlink(file.path(tempdir(), "Logfiles"), recursive = T)
  }
  if (file.exists(file.path(tempdir(), "Graphs"))) {
    unlink(file.path(tempdir(), "Graphs"), recursive = T)
  }
  if (file.exists(file.path(tempdir(), "Graphs"))) {
    unlink(file.path(tempdir(), "files R-Sufi"), recursive = T)
  }
} # funzione RoME
