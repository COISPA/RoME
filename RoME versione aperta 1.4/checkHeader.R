checkHeader <- function(dataframe, template)
{
  
  # Header Before 2012
  
  TA_before_2012 <- c("TYPE_OF_FILE","COUNTRY","AREA","VESSEL","GEAR","RIGGING","DOORS","YEAR","MONTH","DAY","HAUL_NUMBER",
"CODEND_CLOSING","SHOOTING_TIME","SHOOTING_QUADRANT","SHOOTING_LATITUDE","SHOOTING_LONGITUDE","SHOOTING_DEPTH","HAULING_TIME",
"HAULING_QUADRANT","HAULING_LATITUDE","HAULING_LONGITUDE","HAULING_DEPTH","HAUL_DURATION","VALIDITY","COURSE","RECORDED_SPECIES",
"DISTANCE","VERTICAL_OPENING","WING_OPENING","GEOMETRICAL_PRECISION","BRIDLES_LENGTH","WARP_LENGTH","WARP_DIAMETER",
"HYDROLOGICAL_STATION","OBSERVATIONS")

  TB_before_2012 <- c("TYPE_OF_FILE","COUNTRY","AREA","VESSEL","YEAR","HAUL_NUMBER","CODEND_CLOSING","PART_OF_THE_CODEND",
                      "FAUNISTIC_CATEGORY","GENUS","SPECIES","NAME_OF_THE_REFERENCE_LIST","TOTAL_WEIGHT_IN_HAUL",
                      "TOTAL_NUMBER_IN_HAUL","NUMBER_OF_FEMALES","NUMBER_OF_MALES","NUMBER_OF_UNDETERMINED")

  TC_before_2012 <- c("TYPE_OF_FILE","COUNTRY","AREA","VESSEL","YEAR","HAUL_NUMBER","CODEND_CLOSING","PART_OF_THE_CODEND",
                      "GENUS","SPECIES","LENGTH_CLASSES_CODE","WEIGHT_OF_THE_FRACTION","WEIGHT_OF_THE_SAMPLE_MEASURED",
                      "SEX","NO_OF_INDIVIDUAL_OF_THE_ABOVE_SEX_MEASURED","LENGTH_CLASS","MATURITY","MATSUB",
                      "NUMBER_OF_INDIVIDUALS_IN_THE_LENGTH_CLASS_AND_MATURITY_STAGE")

  TD_before_2012 <- c("TYPE_OF_FILE","COUNTRY","AREA","VESSEL","YEAR","HAUL_NUMBER","BOTTOM_TEMPERATURE_BEGINNING",
                      "BOTTOM_TEMPERATURE_END","METHOD")
  
  TT_before_2012 <- c("TYPE_OF_FILE","YEAR","COUNTRY","AREA","VESSEL","HAUL_NUMBER","STRATUM")


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
if(Format=="before_2012")
{
    #eseguo i controlli per il before 2012
    if(template=="TA")
    {
      if (isTRUE(all.equal( colnames(dataframe), TA_before_2012))) {
        # colonne corrette
      } else{
        stop("ERROR: wrong TA headers for before 2012")
        
    }
    }
    
    if(template=="TB")
    {
      if (isTRUE(all.equal( colnames(dataframe), TB_before_2012))) {
        # colonne corrette
      } else{
        stop("ERROR: wrong TB headers for before 2012")
        
      }
    }
    
    if(template=="TC")
    {
      if (isTRUE(all.equal( colnames(dataframe), TC_before_2012))) {
        # colonne corrette
      } else{
        stop("ERROR: wrong TC headers for before 2012")
        
      }
    }
    
    if(template=="TT")
    {
      if (isTRUE(all.equal( colnames(dataframe), TT_before_2012))) {
        # colonne corrette
      } else{
        stop("ERROR: wrong TT headers for before 2012")
        
      }
    }
    
    if(template=="TD")
    {
      if (isTRUE(all.equal( colnames(dataframe), TD_before_2012))) {
        # colonne corrette
      } else{
        stop("ERROR: wrong TD headers for before 2012")
        
      }
    }
      
  
}else{
  
  if(template=="TA")
  {
    if (isTRUE(all.equal( colnames(dataframe), TA_after_2012))) {
      # colonne corrette
    } else{
      stop("ERROR: wrong TA headers for after 2012")
      
    }
  }
  
  if(template=="TB")
  {
    if (isTRUE(all.equal( colnames(dataframe), TB_after_2012))) {
      # colonne corrette
    } else{
      stop("ERROR: wrong TB headers for after 2012")
      
    }
  }
  
  if(template=="TC")
  {
    if (isTRUE(all.equal( colnames(dataframe), TC_after_2012))) {
      # colonne corrette
    } else{
      stop("ERROR: wrong TC headers for after 2012")
      
    }
  }
  
  if(template=="TE")
  {
    if (isTRUE(all.equal( colnames(dataframe), TE_after_2012))) {
      # colonne corrette
    } else{
      stop("ERROR: wrong TE headers for after 2012")
      
    }
  }
  
  if(template=="TL")
  {
    if (isTRUE(all.equal(colnames(dataframe), TL_after_2012))) {
      # colonne corrette
    } else{
      stop("ERROR: wrong TL headers for after 2012")
      
    }
  }
  
}


  
  
  
}