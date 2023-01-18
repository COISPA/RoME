############################################################################################################################
#   RoME: R code to perform multiple checks on MEDITS Survey data (TA, TB, TC and TE files)                                #
#   Authors: I. Bitetto, W. Zupa, M.T. Spedicato                                                                           #
#   Coispa Tecnologia & Ricerca - Stazione sperimentale per lo Studio delle Risorse del Mare                               #
#   If you have any comments or suggestions please contact the following e-mail address: bitetto@coispa.it, zupa@coispa.it #
#   January 2022                                                                                                           #
############################################################################################################################
checkHeader <- function(dataframe, template, wd, suffix)
{

    Format="from_2012"

  if (!file.exists(file.path(wd, "Logfiles"))){
    dir.create(file.path(wd, "Logfiles"), recursive = TRUE, showWarnings = FALSE)
  }
  if (!file.exists(file.path(wd,"Graphs"))){
    dir.create(file.path(wd, "Graphs"), showWarnings = FALSE)
  }
  if (is.na(suffix)){
    suffix <- paste(as.character(Sys.Date()),format(Sys.time(), "_time_h%Hm%Ms%OS0"),sep="")
  }
  Errors <- file.path(wd,"Logfiles",paste("Logfile_",suffix,".dat",sep=""))

  if (!file.exists(Errors)){
    file.create(Errors)
  }

  numberError = 0

  write(paste("\n----------- check of HEADERS"), file = Errors, append = TRUE)



  # Header after 2012

  TA_after_2012 <- c("TYPE_OF_FILE","COUNTRY","AREA","VESSEL","GEAR","RIGGING","DOORS","YEAR","MONTH","DAY",
                     "HAUL_NUMBER","CODEND_CLOSING","PART_OF_THE_CODEND","SHOOTING_TIME","SHOOTING_QUADRANT",
                     "SHOOTING_LATITUDE","SHOOTING_LONGITUDE","SHOOTING_DEPTH","HAULING_TIME","HAULING_QUADRANT",
                     "HAULING_LATITUDE","HAULING_LONGITUDE","HAULING_DEPTH","HAUL_DURATION","VALIDITY","COURSE",
                     "RECORDED_SPECIES","DISTANCE","VERTICAL_OPENING","WING_OPENING","GEOMETRICAL_PRECISION",
                     "BRIDLES_LENGTH","WARP_LENGTH","WARP_DIAMETER","HYDROLOGICAL_STATION","OBSERVATIONS","BOTTOM_TEMPERATURE_BEGINNING",
                     "BOTTOM_TEMPERATURE_END","MEASURING_SYSTEM","NUMBER_OF_THE_STRATUM","BOTTOM_SALINITY_BEGINNING",	"BOTTOM_SALINITY_END","MEASURING_SYSTEM_SALINITY")




  TB_after_2012 <- c("TYPE_OF_FILE","COUNTRY","AREA","VESSEL","YEAR","MONTH","DAY","HAUL_NUMBER","CODEND_CLOSING",
                     "PART_OF_THE_CODEND","FAUNISTIC_CATEGORY","GENUS","SPECIES","NAME_OF_THE_REFERENCE_LIST",
                     "TOTAL_WEIGHT_IN_THE_HAUL","TOTAL_NUMBER_IN_THE_HAUL","NB_OF_FEMALES","NB_OF_MALES","NB_OF_UNDETERMINED")

#   TC_after_2012 <- c("TYPE_OF_FILE","COUNTRY","AREA","VESSEL","YEAR","HAUL_NUMBER","CODEND_CLOSING","PART_OF_THE_CODEND",
#                      "GENUS","SPECIES","LENGTH_CLASSES_CODE","WEIGHT_OF_THE_FRACTION","WEIGHT_OF_THE_SAMPLE_MEASURED","SEX",
#                      "NO_OF_INDIVIDUAL_OF_THE_ABOVE_SEX_MEASURED","LENGTH_CLASS","MATURITY","MATSUB","NUMBER_OF_INDIVIDUALS_IN_THE_LENGTH_CLASS_AND_MATURITY_STAGE")

  TC_after_2012 <- c("TYPE_OF_FILE","COUNTRY","AREA","VESSEL","YEAR","MONTH","DAY","HAUL_NUMBER","CODEND_CLOSING","PART_OF_THE_CODEND",
                     "FAUNISTIC_CATEGORY","GENUS","SPECIES","LENGTH_CLASSES_CODE","WEIGHT_OF_THE_FRACTION",
                     "WEIGHT_OF_THE_SAMPLE_MEASURED","SEX","NO_OF_INDIVIDUAL_OF_THE_ABOVE_SEX_MEASURED","LENGTH_CLASS",
                     "MATURITY","MATSUB","NUMBER_OF_INDIVIDUALS_IN_THE_LENGTH_CLASS_AND_MATURITY_STAGE")

#   TE_after_2012 <- c("TYPE_OF_FILE","COUNTRY","AREA","VESSEL","YEAR","MONTH","DAY","HAUL_NUMBER","FAUNISTIC_CATEGORY",
#                      "GENUS","SPECIES","LENGTH_CLASSES_CODE","SEX","NO_PER_SEX_MEASURED_IN_SUB_SAMPLE_FOR_OTOLITH",
#                      "LENGTH_CLASS","MATURITY","MATSUB","INDIVIDUAL_WEIGHT","NO_PER_SEX_MEASURED_IN_SUB_SAMPLE_FOR_WEIGHT",
#                      "OTOLITH_SAMPLED","NO_PER_SEX_MEASURED_IN_SUBSAMPLE_FOR_AGEING","OTOLITH_READ","AGE","OTOLITH_CODE")
#
  TE_after_2012 <- c("TYPE_OF_FILE","COUNTRY","AREA","VESSEL","YEAR","MONTH","DAY","HAUL_NUMBER","FAUNISTIC_CATEGORY",
                     "GENUS","SPECIES","LENGTH_CLASSES_CODE","SEX","NO_PER_SEX_MEASURED_IN_SUB_SAMPLE_FOR_OTOLITH",
                     "LENGTH_CLASS","MATURITY","MATSUB","INDIVIDUAL_WEIGHT","NO_PER_SEX_MEASURED_IN_SUB_SAMPLE_FOR_WEIGHT",
                     "OTOLITH_SAMPLED","NO_PER_SEX_MEASURED_IN_SUB_SAMPLE_FOR_AGEING","OTOLITH_READ","AGE","OTOLITH_CODE","RECORD_NUMBER")

  TL_after_2012 <- c("TYPE_OF_FILE", "COUNTRY",	"AREA", "VESSEL",	"YEAR",	"MONTH",	"DAY",	"HAUL_NUMBER",	"LITTER_CATEGORY","LITTER_SUB-CATEGORY",
                     "TOTAL_WEIGHT_IN_THE_CATEGORY_HAUL",	"TOTAL_NUMBER_IN_THE_CATEGORY_HAUL","TOTAL_WEIGHT_IN_THE_SUB-CATEGORY_HAUL",
                     "TOTAL_NUMBER_IN_THE_SUB-CATEGORY_HAUL")


  #### CHECK TL FIELDS ####
  {
    if ("LITTER_SUB.CATEGORY" %in% colnames(dataframe)){
      colnames(dataframe)[which(colnames(dataframe)=="LITTER_SUB.CATEGORY")] <- "LITTER_SUB-CATEGORY"
    }
    if ("TOTAL_WEIGHT_IN_THE_SUB.CATEGORY_HAUL" %in% colnames(dataframe)){
      colnames(dataframe)[which(colnames(dataframe)=="TOTAL_WEIGHT_IN_THE_SUB.CATEGORY_HAUL")] <- "TOTAL_WEIGHT_IN_THE_SUB-CATEGORY_HAUL"
    }
    if ("TOTAL_NUMBER_IN_THE_SUB.CATEGORY_HAUL" %in% colnames(dataframe)){
      colnames(dataframe)[which(colnames(dataframe)=="TOTAL_NUMBER_IN_THE_SUB.CATEGORY_HAUL")] <- "TOTAL_NUMBER_IN_THE_SUB-CATEGORY_HAUL"
    }
  }
  #### CHECK TL FIELDS - END ####


  # eseguo i controlli
  if(template=="TA")
  {
    if (isTRUE(all.equal( colnames(dataframe), TA_after_2012))) {
      # colonne corrette
    } else{
      write(paste("ERROR: wrong TA headers. RoME checks will not be performed further until the error is corrected."), file = Errors, append = TRUE)
      numberError=numberError+1
    }
  }

  if(template=="TB")
  {
    if (isTRUE(all.equal( colnames(dataframe), TB_after_2012))) {
      # colonne corrette
    } else{
      write(paste("ERROR: wrong TB headers. RoME checks will not be performed further until the error is corrected."), file = Errors, append = TRUE)
      numberError=numberError+1
    }
  }

  if(template=="TC")
  {
    if (isTRUE(all.equal( colnames(dataframe), TC_after_2012))) {
      # colonne corrette
    } else{
      write(paste("ERROR: wrong TC headers. RoME checks will not be performed further until the error is corrected."), file = Errors, append = TRUE)
      numberError=numberError+1
    }
  }

  if(template=="TE")
  {
    if (isTRUE(all.equal( colnames(dataframe), TE_after_2012))) {
      # colonne corrette
    } else{
      write(paste("ERROR: wrong TE headers. RoME checks will not be performed further until the error is corrected."), file = Errors, append = TRUE)
      numberError=numberError+1
    }
  }

  if(template=="TL")
  {
    if (isTRUE(all.equal(colnames(dataframe), TL_after_2012))) {
      # colonne corrette
    } else{
      write(paste("ERROR: wrong TL headers. RoME checks will not be performed further until the error is corrected."), file = Errors, append = TRUE)
      numberError=numberError+1
    }
  }

  if (numberError ==0) {
    write(paste("No error occurred"), file = Errors, append = TRUE)
  }
  if (file.exists(file.path(tempdir(), "Logfiles"))){
    unlink(file.path(tempdir(),"Logfiles"),recursive=T)
  }
  if (file.exists(file.path(tempdir(), "Graphs"))){
    unlink(file.path(tempdir(),"Graphs"),recursive=T)
  }
  if (file.exists(file.path(tempdir(), "files R-Sufi"))){
    unlink(file.path(tempdir(),"files R-Sufi"),recursive=T)
  }
  if (numberError ==0) {
    return(TRUE)
  } else {
    return(FALSE)
  }

}
