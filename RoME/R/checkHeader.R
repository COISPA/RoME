###########################################################################################################################
#   RoME: R code to perform multiple checks on MEDITS Survey data (TA, TB, TC and TE files - old and new MEDITS formats)                                   #
#   Authors: I. Bitetto, W. Zupa, M.T. Spedicato                                                                    #
#   Coispa Tecnologia & Ricerca - Stazione sperimentale per lo Studio delle Risorse del Mare                              #
#   If you have any comments or suggestions please contact the following e-mail address: bitetto@coispa.it                #
#   March 2020
################################################################################

checkHeader <- function(dataframe, template)
{

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

  TL_after_2012 <- c("TYPE_OF_FILE", "COUNTRY",	"AREA", "VESSEL",	"YEAR",	"MONTH",	"DAY",	"HAUL_NUMBER",	"LITTER_CATEGORY","LITTER_SUB.CATEGORY",
                     "TOTAL_WEIGHT_IN_THE_CATEGORY_HAUL",	"TOTAL_NUMBER_IN_THE_CATEGORY_HAUL","TOTAL_WEIGHT_IN_THE_SUB.CATEGORY_HAUL",
                     "TOTAL_NUMBER_IN_THE_SUB.CATEGORY_HAUL")



  # eseguo i controlli


  if(template=="TA")
  {
    if (isTRUE(all.equal( colnames(dataframe), TA_after_2012))) {
      # colonne corrette
    } else{
      stop("ERROR: wrong TA headers.")

    }
  }

  if(template=="TB")
  {
    if (isTRUE(all.equal( colnames(dataframe), TB_after_2012))) {
      # colonne corrette
    } else{
      stop("ERROR: wrong TB headers.")

    }
  }

  if(template=="TC")
  {
    if (isTRUE(all.equal( colnames(dataframe), TC_after_2012))) {
      # colonne corrette
    } else{
      stop("ERROR: wrong TC headers.")

    }
  }

  if(template=="TE")
  {
    if (isTRUE(all.equal( colnames(dataframe), TE_after_2012))) {
      # colonne corrette
    } else{
      stop("ERROR: wrong TE headers.")

    }
  }

  if(template=="TL")
  {
    if (isTRUE(all.equal(colnames(dataframe), TL_after_2012))) {
      # colonne corrette
    } else{
      stop("ERROR: wrong TL headers.")

    }
  }



}
