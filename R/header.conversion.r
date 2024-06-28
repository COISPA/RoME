#' Headers conversion for MEDITS tables
#'
#' @param table data frame of the TX table
#' @param type type of tables to be analysed. Allowed values: "TA","TB","TC","TE","TL"
#' @param verbose boolean. If TRUE a message is printed.
#' @details The functions allow to convert headers of table coming from RDBFIS data base to the MEDITS format expected from RoME package
#' @return A data frame object is returned including the only allowed field
headers.conversion <- function(table,type,verbose=FALSE) {

  headers <- structure(list(id = 1:129, table = c(
    "TA", "TA", "TA", "TA",
    "TA", "TA", "TA", "TA", "TA", "TA", "TA", "TA", "TA", "TA", "TA",
    "TA", "TA", "TA", "TA", "TA", "TA", "TA", "TA", "TA", "TA", "TA",
    "TA", "TA", "TA", "TA", "TA", "TA", "TA", "TA", "TA", "TA", "TA",
    "TA", "TA", "TA", "TA", "TA", "TA", "TA", "TB", "TB", "TB", "TB",
    "TB", "TB", "TB", "TB", "TB", "TB", "TB", "TB", "TB", "TB", "TB",
    "TB", "TB", "TB", "TB", "TB", "TC", "TC", "TC", "TC", "TC", "TC",
    "TC", "TC", "TC", "TC", "TC", "TC", "TC", "TC", "TC", "TC", "TC",
    "TC", "TC", "TC", "TC", "TC", "TC", "TE", "TE", "TE", "TE", "TE",
    "TE", "TE", "TE", "TE", "TE", "TE", "TE", "TE", "TE", "TE", "TE",
    "TE", "TE", "TE", "TE", "TE", "TE", "TE", "TE", "TE", "TE", "TL",
    "TL", "TL", "TL", "TL", "TL", "TL", "TL", "TL", "TL", "TL", "TL",
    "TL", "TL", "TL", "TL"
  ), RDBFIS = c(
    "name_of_survey", "type_of_file",
    "country", "area", "vessel", "gear", "rigging", "doors", "year",
    "month", "day", "haul_number", "codend_closing", "part_of_the_codend",
    "shooting_time", "shooting_quadrant", "shooting_latitude", "shooting_longitude",
    "shooting_depth", "hauling_time", "hauling_quadrant", "hauling_latitude",
    "hauling_longitude", "hauling_depth", "haul_duration", "validity",
    "course", "recorded_species", "distance", "vertical_opening",
    "wing_opening", "geometrical_precision", "bridles_length", "warp_length",
    "warp_diameter", "hydrological_station", "observations", "bottom_temperature_beginning",
    "bottom_temperature_end", "measuring_system", "number_of_the_stratum",
    "bottom_salinity_beginning", "bottom_salinity_end", "measuring_system_salinity",
    "name_of_survey", "type_of_file", "country", "area", "vessel",
    "year", "month", "day", "haul_number", "codend_closing", "part_of_the_codend",
    "faunistic_category", "genus", "species", "name_of_the_reference_list",
    "total_weight_in_the_haul", "total_number_in_the_haul", "nb_of_females",
    "nb_of_males", "nb_of_undetermined", "name_of_survey", "type_of_file",
    "country", "area", "vessel", "year", "month", "day", "haul_number",
    "codend_closing", "part_of_the_codend", "faunistic_category",
    "genus", "species", "length_classes_code", "weight_of_the_fraction",
    "weight_of_the_sample_measured", "sex", "no_of_individual_of_the_above_sex_measured",
    "length_class", "maturity", "matsub", "number_of_individuals_in_the_length_class_and_maturity_stage",
    "name_of_survey", "type_of_file", "country", "area", "vessel",
    "year", "month", "day", "haul_number", "faunistic_category",
    "genus", "species", "length_classes_code", "sex", "no_per_sex_measured_in_sub_sample_for_otolith",
    "length_class", "maturity", "matsub", "individual_weight", "no_per_sex_measured_in_sub_sample_for_weight",
    "otolith_sampled", "no_per_sex_measured_in_sub_sample_for_ageing",
    "otolith_read", "age", "otolith_code", "record_number", "name_of_survey",
    "type_of_file", "country", "area", "vessel", "year", "month",
    "day", "haul_number", "litter_category", "litter_sub_category",
    "total_weight_in_the_category_haul", "total_number_in_the_category_haul",
    "total_weight_in_the_sub_category_haul", "total_number_in_the_sub_category_haul", "notes"
  ), MEDITS = c(
    NA, "TYPE_OF_FILE", "COUNTRY", "AREA", "VESSEL",
    "GEAR", "RIGGING", "DOORS", "YEAR", "MONTH", "DAY", "HAUL_NUMBER",
    "CODEND_CLOSING", "PART_OF_THE_CODEND", "SHOOTING_TIME", "SHOOTING_QUADRANT",
    "SHOOTING_LATITUDE", "SHOOTING_LONGITUDE", "SHOOTING_DEPTH",
    "HAULING_TIME", "HAULING_QUADRANT", "HAULING_LATITUDE", "HAULING_LONGITUDE",
    "HAULING_DEPTH", "HAUL_DURATION", "VALIDITY", "COURSE", "RECORDED_SPECIES",
    "DISTANCE", "VERTICAL_OPENING", "WING_OPENING", "GEOMETRICAL_PRECISION",
    "BRIDLES_LENGTH", "WARP_LENGTH", "WARP_DIAMETER", "HYDROLOGICAL_STATION",
    "OBSERVATIONS", "BOTTOM_TEMPERATURE_BEGINNING", "BOTTOM_TEMPERATURE_END",
    "MEASURING_SYSTEM", "NUMBER_OF_THE_STRATUM", "BOTTOM_SALINITY_BEGINNING",
    "BOTTOM_SALINITY_END", "MEASURING_SYSTEM_SALINITY", NA, "TYPE_OF_FILE",
    "COUNTRY", "AREA", "VESSEL", "YEAR", "MONTH", "DAY", "HAUL_NUMBER",
    "CODEND_CLOSING", "PART_OF_THE_CODEND", "FAUNISTIC_CATEGORY",
    "GENUS", "SPECIES", "NAME_OF_THE_REFERENCE_LIST", "TOTAL_WEIGHT_IN_THE_HAUL",
    "TOTAL_NUMBER_IN_THE_HAUL", "NB_OF_FEMALES", "NB_OF_MALES", "NB_OF_UNDETERMINED",
    NA, "TYPE_OF_FILE", "COUNTRY", "AREA", "VESSEL", "YEAR", "MONTH",
    "DAY", "HAUL_NUMBER", "CODEND_CLOSING", "PART_OF_THE_CODEND",
    "FAUNISTIC_CATEGORY", "GENUS", "SPECIES", "LENGTH_CLASSES_CODE",
    "WEIGHT_OF_THE_FRACTION", "WEIGHT_OF_THE_SAMPLE_MEASURED", "SEX",
    "NO_OF_INDIVIDUAL_OF_THE_ABOVE_SEX_MEASURED", "LENGTH_CLASS",
    "MATURITY", "MATSUB", "NUMBER_OF_INDIVIDUALS_IN_THE_LENGTH_CLASS_AND_MATURITY_STAGE",
    NA, "TYPE_OF_FILE", "COUNTRY", "AREA", "VESSEL", "YEAR", "MONTH",
    "DAY", "HAUL_NUMBER", "FAUNISTIC_CATEGORY", "GENUS", "SPECIES",
    "LENGTH_CLASSES_CODE", "SEX", "NO_PER_SEX_MEASURED_IN_SUB_SAMPLE_FOR_OTOLITH",
    "LENGTH_CLASS", "MATURITY", "MATSUB", "INDIVIDUAL_WEIGHT", "NO_PER_SEX_MEASURED_IN_SUB_SAMPLE_FOR_WEIGHT",
    "OTOLITH_SAMPLED", "NO_PER_SEX_MEASURED_IN_SUB_SAMPLE_FOR_AGEING",
    "OTOLITH_READ", "AGE", "OTOLITH_CODE", "RECORD_NUMBER", NA, "TYPE_OF_FILE",
    "COUNTRY", "AREA", "VESSEL", "YEAR", "MONTH", "DAY", "HAUL_NUMBER",
    "LITTER_CATEGORY", "LITTER_SUB-CATEGORY", "TOTAL_WEIGHT_IN_THE_CATEGORY_HAUL",
    "TOTAL_NUMBER_IN_THE_CATEGORY_HAUL", "TOTAL_WEIGHT_IN_THE_SUB-CATEGORY_HAUL",
    "TOTAL_NUMBER_IN_THE_SUB-CATEGORY_HAUL", NA
  ), class = c(
    NA, "character",
    "character", "integer", "character", "character", "character",
    "character", "integer", "integer", "integer", "integer", "character",
    "character", "integer", "integer", "numeric", "numeric", "integer",
    "integer", "integer", "numeric", "numeric", "integer", "integer",
    "character", "character", "integer", "integer", "integer", "integer",
    "character", "integer", "integer", "integer", "character", "integer",
    "numeric", "numeric", "character", "character", "numeric", "numeric",
    "character", NA, "character", "character", "integer", "character",
    "integer", "integer", "integer", "integer", "character", "character",
    "character", "character", "character", "character", "integer",
    "integer", "integer", "integer", "integer", NA, "character",
    "character", "integer", "character", "integer", "integer", "integer",
    "integer", "character", "character", "character", "character",
    "character", "character", "integer", "integer", "character",
    "integer", "integer", "character", "character", "integer", NA,
    "character", "character", "integer", "character", "integer",
    "integer", "integer", "integer", "character", "character", "character",
    "character", "character", "integer", "integer", "integer", "character",
    "character", "integer", "character", "integer", "character",
    "character", "character", "integer", NA, "character", "character",
    "integer", "character", "integer", "integer", "integer", "integer",
    "character", "character", "integer", "integer", "integer", "integer", NA
  )), class = "data.frame", row.names = c(NA, -129L))

  if (FALSE) {
    table <- tl
    type <- "TL"
  }

  if (!type %in% c("TA","TB","TC","TE","TL") ) {
    stop("Not expected type of table")
  }

  ### TA
  if (type =="TA") {
     headers <- headers[headers$table == type, ]
     if ("name_of_survey" %in% colnames(table)){
        table <- table[ ,-(which(colnames(table)=="name_of_survey"))]
     }
     if ("name_of_survey"%in%headers$RDBFIS){
        headers <- headers[-(which(headers$RDBFIS=="name_of_survey")) , ]
     }
     if (all(colnames(table)==headers$RDBFIS)) {
         c<-1
       for (c in 1:ncol(table)) {
         # print(c)
         colnames(table)[c] <- headers$MEDITS[c]
         if (headers$class[c] %in% c("interger","numeric") & (class(table[,c])!=headers$class[c])) {
           class(table[,c]) <- headers$class[c]
           if (verbose) {
             print(paste0("- class modified: ",headers$RDBFIS,"\n"))
           }
         }
       }
     }  else { # all(colnames(table)==headers$RDBFIS)
           message("Unexpected headers found in the provided TA table. The conversion was not finalised")
        }
     } # type=="TA"

  ### TB
  if (type =="TB") {
    headers <- headers[headers$table == type, ]
    if ("name_of_survey" %in% colnames(table)){
      table <- table[ ,-(which(colnames(table)=="name_of_survey"))]
    }
    if ("name_of_survey"%in%headers$RDBFIS){
      headers <- headers[-(which(headers$RDBFIS=="name_of_survey")) , ]
    }
    if (all(colnames(table)==headers$RDBFIS)) {
      c<-1
      for (c in 1:ncol(table)) {
        # print(c)
        colnames(table)[c] <- headers$MEDITS[c]
        if (headers$class[c] %in% c("interger","numeric") & (class(table[,c])!=headers$class[c])) {
          class(table[,c]) <- headers$class[c]
          if (verbose) {
            print(paste0("- class modified: ",headers$RDBFIS,"\n"))
          }
        }
      }
    }  else { # all(colnames(table)==headers$RDBFIS)
      message("Unexpected headers found in the provided TB table. The conversion was not finalised")
    }
  }

  ### TC
  if (type =="TC") {
    headers <- headers[headers$table == type, ]
    if ("name_of_survey" %in% colnames(table)){
      table <- table[ ,-(which(colnames(table)=="name_of_survey"))]
    }
    if ("name_of_survey"%in%headers$RDBFIS){
      headers <- headers[-(which(headers$RDBFIS=="name_of_survey")) , ]
    }
    if (all(colnames(table)==headers$RDBFIS)) {
      c<-1
      for (c in 1:ncol(table)) {
        # print(c)
        colnames(table)[c] <- headers$MEDITS[c]
        if (headers$class[c] %in% c("interger","numeric") & (class(table[,c])!=headers$class[c])) {
          class(table[,c]) <- headers$class[c]
          if (verbose) {
            print(paste0("- class modified: ",headers$RDBFIS,"\n"))
          }
        }
      }
    }  else { # all(colnames(table)==headers$RDBFIS)
      message("Unexpected headers found in the provided TC table. The conversion was not finalised")
    }
  }
  #########

  ### TE
  if (type =="TE") {
    headers <- headers[headers$table == type, ]
    if ("name_of_survey" %in% colnames(table)){
      table <- table[ ,-(which(colnames(table)=="name_of_survey"))]
    }
    if ("name_of_survey"%in%headers$RDBFIS){
      headers <- headers[-(which(headers$RDBFIS=="name_of_survey")) , ]
    }
    if (all(colnames(table)==headers$RDBFIS)) {
      c<-1
      for (c in 1:ncol(table)) {
        # print(c)
        colnames(table)[c] <- headers$MEDITS[c]
        if (headers$class[c] %in% c("interger","numeric") & (class(table[,c])!=headers$class[c])) {
          class(table[,c]) <- headers$class[c]
          if (verbose) {
            print(paste0("- class modified: ",headers$RDBFIS,"\n"))
          }
        }
      }
    }  else { # all(colnames(table)==headers$RDBFIS)
      message("Unexpected headers found in the provided TE table. The conversion was not finalised")
    }
  }
  #########

  ### TL
  if (type =="TL") {
    headers <- headers[headers$table == type, ]
    if ("name_of_survey" %in% colnames(table)){
      table <- table[ ,-(which(colnames(table)=="name_of_survey"))]
    }
    if ("notes" %in% colnames(table)){
      table <- table[ ,-(which(colnames(table)=="notes"))]
    }
    if ("name_of_survey"%in%headers$RDBFIS){
      headers <- headers[-(which(headers$RDBFIS=="name_of_survey")) , ]
    }
    if ("notes"%in%headers$RDBFIS){
      headers <- headers[-(which(headers$RDBFIS=="notes")) , ]
    }
    if (all(colnames(table)==headers$RDBFIS)) {
      c<-1
      for (c in 1:ncol(table)) {
        # print(c)
        colnames(table)[c] <- headers$MEDITS[c]
        if (headers$class[c] %in% c("interger","numeric") & (class(table[,c])!=headers$class[c])) {
          class(table[,c]) <- headers$class[c]
          if (verbose) {
            print(paste0("- class modified: ",headers$RDBFIS,"\n"))
          }
        }
      }
    }  else { # all(colnames(table)==headers$RDBFIS)
      message("Unexpected headers found in the provided TL table. The conversion was not finalised")
    }
  }
  #########

  return(table)
}
