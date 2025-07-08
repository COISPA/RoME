# Check if all the target species in TB are present in TC

if (FALSE){
  ResultTC = RoME::TC
  ResultTB = RoME::TB
  year=2010
  wd <- tempdir()
  DataSpecies=RoME::DataTargetSpecies
  suffix=paste(as.character(Sys.Date()),format(Sys.time(), "_time_h%Hm%Ms%OS0"),sep="")
  # load("C:/Users/Bitetto Isabella/OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L/Rome/ROME//RoME//data//DataTargetSpecies.rda")
  # load("C:/Users/Bitetto Isabella/OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L/Rome/ROME//RoME//data//Maturity_parameters.rda")
  # load("C:/Users/Bitetto Isabella/OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L/Rome/ROME//RoME//data//TM_list.rda")
  check_species_TBTC(ResultTB,ResultTC,year,DataSpecies,wd,suffix)
  }

check_species_TBTC <- function(
    ResultTB,
    ResultTC,
    year,
    DataSpecies = RoME::DataTargetSpecies,
    wd,
    suffix
) {

  if (!file.exists(file.path(wd, "Logfiles"))) {
    dir.create(file.path(wd, "Logfiles"), recursive = TRUE, showWarnings = FALSE)
  }

  if (!exists("suffix")) {
    suffix <- paste(
      as.character(Sys.Date()),
      format(Sys.time(), "_time_h%Hm%Ms%OS0"),
      sep = ""
    )
  }

  Errors <- file.path(
    wd, "Logfiles",
    paste("Logfile_", suffix, ".dat", sep = "")
  )

  if (!file.exists(Errors)) {
    file.create(Errors)
  }

  # Filter for selected year
  ResultTB <- ResultTB[ResultTB$YEAR == year, ]
  ResultTC <- ResultTC[ResultTC$YEAR == year, ]

  # Check if either TB or TC is empty
  if (nrow(ResultTB) == 0 || nrow(ResultTC) == 0) {
    if (nrow(ResultTB) == 0 && nrow(ResultTC) == 0) {
      write(
        paste("Both TB and TC are empty for selected year:", year),
        file = Errors,
        append = TRUE
      )
    } else if (nrow(ResultTB) == 0) {
      write(
        paste("TB is empty for selected year:", year),
        file = Errors,
        append = TRUE
      )
    } else if (nrow(ResultTC) == 0) {
      write(
        paste("TC is empty for selected year:", year),
        file = Errors,
        append = TRUE
      )
    }
    return(TRUE)
  }

  write(
    paste(
      "\n----------- check presence in TC of TB target species -", year
    ),
    file = Errors,
    append = TRUE
  )

  ResultTB <- ResultTB[, c("YEAR", "HAUL_NUMBER", "GENUS", "SPECIES")]
  ResultTC <- ResultTC[, c("YEAR", "HAUL_NUMBER", "GENUS", "SPECIES")]

  # Determine target species
  ResultSpecies <- DataSpecies

  # Filter target species:
  # target if:
  #  - has START_YEAR (not NA), or
  #  - GROUP is G1 or G2
  # otherwise not target
  Target <- ResultSpecies[
    !is.na(ResultSpecies$START_YEAR) |
      (ResultSpecies$GROUP %in% c("G1", "G2")),
  ]

  # Further filter based on END_YEAR
  if (nrow(Target) > 0) {
    Target <- Target[
      is.na(Target$END_YEAR) |
        Target$END_YEAR > year,
    ]
  }

  # Only species with START_YEAR <= year are valid
  Target <- Target[
    is.na(Target$START_YEAR) | Target$START_YEAR <= year,
  ]

  # Check each species in TB
  if (nrow(ResultTB) > 0) {
    for (j in 1:nrow(ResultTB)) {
      StrSpecies <- paste(ResultTB$GENUS[j], ResultTB$SPECIES[j], sep = "")
      FoundTarget <- Target[Target$SPECIES == StrSpecies, ]
      if (nrow(FoundTarget) != 0) {
        # Only check presence in TC if species is target
        FoundInTC <- ResultTC[
          ResultTC$GENUS == ResultTB$GENUS[j] &
            ResultTC$SPECIES == ResultTB$SPECIES[j] &
            ResultTC$HAUL_NUMBER == ResultTB$HAUL_NUMBER[j],
        ]

        if (nrow(FoundInTC) == 0) {
          # Compose warning message
          warning_msg <- paste(
            "Warning:",
            StrSpecies,
            "not found in TC for haul",
            ResultTB$HAUL_NUMBER[j],
            "(year", year, ")"
          )
          warning(warning_msg)

          # Optionally write each warning to the log file:
          # Uncomment below if you want one warning per line in .dat
          # write(
          #   warning_msg,
          #   file = Errors,
          #   append = TRUE
          # )
        }
      }
    }
  }

  write(
    paste("Check completed for year", year, "- No errors detected."),
    file = Errors,
    append = TRUE
  )

  return(TRUE)
}

