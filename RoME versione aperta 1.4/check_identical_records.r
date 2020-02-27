###########################################################################################################################
#   RoME: R code to perform multiple checks on MEDITS Survey data (TA, TB, TC and TE files - old and new MEDITS formats)                                   #                   
#   Authors: I. Bitetto, M.T. Facchini, M.T. Spedicato                                                                    #
#   Coispa Tecnologia & Ricerca - Stazione sperimentale per lo Studio delle Risorse del Mare                              #
#   If you have any comments or suggestions please contact the following e-mail address: bitetto@coispa.it                #                                                                                  
#   March 2013                                                                                                            #
###########################################################################################################################
# Check identical records
###########################################################################################################################################

check_identical_records<-function(ResultData, Table){
  check_without_errors = FALSE
  
  
   Result = ResultData # read.csv(paste(getwd(),"/",Data,".csv",sep=""), sep=";", header=TRUE)
#   if (Type_of_files==".csv"){
#     write.xlsx(Result,file=paste(Data,".xls", sep = ""))}
  
  if  (Table == "TA")  {
    write(paste("
                ----------- check identical records - ", Result$YEAR[1]), file = Errors, append = TRUE) 
    write(paste("TA: "), file = Errors, append = TRUE)
    
#     channel <- odbcConnectExcel(paste(Data,".xls", sep = ""))
    
    if (Format=="before_2012") { # OLD FORMAT TA

      query= "select count(*) as count, TYPE_OF_FILE, COUNTRY, AREA, VESSEL, GEAR, RIGGING, DOORS, YEAR, MONTH, DAY, HAUL_NUMBER,CODEND_CLOSING, SHOOTING_TIME, SHOOTING_QUADRANT, SHOOTING_LATITUDE, SHOOTING_LONGITUDE, SHOOTING_DEPTH, HAULING_TIME, HAULING_QUADRANT, HAULING_LATITUDE, HAULING_LONGITUDE, HAULING_DEPTH, HAUL_DURATION, VALIDITY, COURSE, RECORDED_SPECIES, DISTANCE, VERTICAL_OPENING, WING_OPENING, GEOMETRICAL_PRECISION, BRIDLES_LENGTH, WARP_LENGTH, WARP_DIAMETER, HYDROLOGICAL_STATION, OBSERVATIONS from Result Group by TYPE_OF_FILE, COUNTRY, AREA, VESSEL, GEAR, RIGGING, DOORS, YEAR, MONTH, DAY, HAUL_NUMBER,CODEND_CLOSING, SHOOTING_TIME, SHOOTING_QUADRANT, SHOOTING_LATITUDE, SHOOTING_LONGITUDE, SHOOTING_DEPTH, HAULING_TIME, HAULING_QUADRANT, HAULING_LATITUDE, HAULING_LONGITUDE, HAULING_DEPTH, HAUL_DURATION, VALIDITY, COURSE, RECORDED_SPECIES, DISTANCE, VERTICAL_OPENING, WING_OPENING, GEOMETRICAL_PRECISION, BRIDLES_LENGTH, WARP_LENGTH, WARP_DIAMETER, HYDROLOGICAL_STATION, OBSERVATIONS order by HAUL_NUMBER"
    
    } else { # NEW FORMAT TA
      
      query= "select count(*) as count, TYPE_OF_FILE, COUNTRY, AREA, VESSEL, GEAR, RIGGING, DOORS, YEAR, MONTH, DAY, HAUL_NUMBER,CODEND_CLOSING, PART_OF_THE_CODEND, SHOOTING_TIME, SHOOTING_QUADRANT, SHOOTING_LATITUDE, SHOOTING_LONGITUDE, SHOOTING_DEPTH, HAULING_TIME, HAULING_QUADRANT, HAULING_LATITUDE, HAULING_LONGITUDE, HAULING_DEPTH, HAUL_DURATION, VALIDITY, COURSE, RECORDED_SPECIES, DISTANCE, VERTICAL_OPENING, WING_OPENING, GEOMETRICAL_PRECISION, BRIDLES_LENGTH, WARP_LENGTH, WARP_DIAMETER, HYDROLOGICAL_STATION, OBSERVATIONS, BOTTOM_TEMPERATURE_BEGINNING, BOTTOM_TEMPERATURE_END, MEASURING_SYSTEM,  NUMBER_OF_THE_STRATUM from Result Group by TYPE_OF_FILE, COUNTRY, AREA, VESSEL, GEAR, RIGGING, DOORS, YEAR, MONTH, DAY, HAUL_NUMBER,CODEND_CLOSING, PART_OF_THE_CODEND, SHOOTING_TIME, SHOOTING_QUADRANT, SHOOTING_LATITUDE, SHOOTING_LONGITUDE, SHOOTING_DEPTH, HAULING_TIME, HAULING_QUADRANT, HAULING_LATITUDE, HAULING_LONGITUDE, HAULING_DEPTH, HAUL_DURATION, VALIDITY, COURSE, RECORDED_SPECIES, DISTANCE, VERTICAL_OPENING, WING_OPENING, GEOMETRICAL_PRECISION, BRIDLES_LENGTH, WARP_LENGTH, WARP_DIAMETER, HYDROLOGICAL_STATION, OBSERVATIONS, BOTTOM_TEMPERATURE_BEGINNING, BOTTOM_TEMPERATURE_END, MEASURING_SYSTEM,  NUMBER_OF_THE_STRATUM order by HAUL_NUMBER" 
          
    }
    
#     Matrix=sqlQuery(channel, query)
    #Matrix=read.csv.sql(paste(getwd(),"/",Data,".csv",sep=""), query, header=T)[[1]]
    Matrix=sqldf(query)
#     odbcClose(channel)
    Matrix= Matrix[Matrix$VALIDITY == "V",]
    Err=which(Matrix$count>1)
    Err=Matrix$HAUL_NUMBER[Err]
    
    if (length(Err)!=0){
      for (j in 1:length(Err)){
        write(paste("Haul ",Err[j],": identical records in", Matrix$TYPE_OF_FILE[1]), file = Errors, append = TRUE)
      }
    }   else
    {
      write(paste("No error occurred"), file = Errors, append = TRUE)  
      check_without_errors = TRUE
    }
  } else if  (Table == "TB")  {
    write(paste("TB:"), file = Errors, append = TRUE)
#     channel <- odbcConnectExcel(paste(Data,".xls", sep = ""))   
    if (Format=="before_2012") {
    query=paste("select count(*) as count, TYPE_OF_FILE, COUNTRY, AREA, VESSEL, YEAR, HAUL_NUMBER, CODEND_CLOSING, PART_OF_THE_CODEND, FAUNISTIC_CATEGORY, GENUS, SPECIES, NAME_OF_THE_REFERENCE_LIST from Result Group by TYPE_OF_FILE, COUNTRY, AREA, VESSEL, YEAR, HAUL_NUMBER, CODEND_CLOSING, PART_OF_THE_CODEND, FAUNISTIC_CATEGORY, GENUS, SPECIES, NAME_OF_THE_REFERENCE_LIST order by HAUL_NUMBER", sep="") 
    } else {
    query=paste("select count(*) as count, TYPE_OF_FILE, COUNTRY, AREA, VESSEL, YEAR, MONTH, DAY, HAUL_NUMBER, CODEND_CLOSING, PART_OF_THE_CODEND, FAUNISTIC_CATEGORY, GENUS, SPECIES, NAME_OF_THE_REFERENCE_LIST from Result Group by TYPE_OF_FILE, COUNTRY, AREA, VESSEL, YEAR, MONTH, DAY, HAUL_NUMBER, CODEND_CLOSING, PART_OF_THE_CODEND, FAUNISTIC_CATEGORY, GENUS, SPECIES, NAME_OF_THE_REFERENCE_LIST order by HAUL_NUMBER", sep="")   
    }
    #Matrix=sqlQuery(channel, query)
    Matrix=sqldf(query)
   # odbcClose(channel)
    Err=which(Matrix$count>1)
    Err=cbind(Matrix$HAUL_NUMBER[Err],as.character(Matrix$GENUS[Err]),as.character(Matrix$SPECIES[Err]))
    if (nrow(Err)!=0){
      for (j in 1:nrow(Err)){
        write(paste("Haul",Err[j,1],Err[j,2],Err[j,3],": identical records in", Matrix$TYPE_OF_FILE[1]), file = Errors, append = TRUE) 
      }
    } else
    {
      write(paste("No error occurred"), file = Errors, append = TRUE)  
      check_without_errors = TRUE
    }
  } else if (Table == "TC") {
    write(paste("TC:"), file = Errors, append = TRUE)
    #channel <- odbcConnectExcel(paste(Data,".xls", sep = ""))
    if (Format=="before_2012") {
    query=paste("select count(*) as count,TYPE_OF_FILE,  COUNTRY,	AREA,	VESSEL,	YEAR,	HAUL_NUMBER,	CODEND_CLOSING,	PART_OF_THE_CODEND,	GENUS,	SPECIES,	LENGTH_CLASSES_CODE,	WEIGHT_OF_THE_FRACTION,	WEIGHT_OF_THE_SAMPLE_MEASURED,	SEX,	NO_OF_INDIVIDUAL_OF_THE_ABOVE_SEX_MEASURED,	LENGTH_CLASS,	MATURITY,	MATSUB from Result Group by TYPE_OF_FILE,	COUNTRY,	AREA,	VESSEL,	YEAR,	HAUL_NUMBER,	CODEND_CLOSING,	PART_OF_THE_CODEND,	GENUS,	SPECIES,	LENGTH_CLASSES_CODE,	WEIGHT_OF_THE_FRACTION,	WEIGHT_OF_THE_SAMPLE_MEASURED,	SEX,	NO_OF_INDIVIDUAL_OF_THE_ABOVE_SEX_MEASURED,	LENGTH_CLASS,	MATURITY,	MATSUB", sep="") 
    } else {
      
      query=paste("select count(*) as count,TYPE_OF_FILE,  COUNTRY,  AREA,	VESSEL,	YEAR,	MONTH,  DAY, HAUL_NUMBER,	CODEND_CLOSING,	PART_OF_THE_CODEND,	FAUNISTIC_CATEGORY, GENUS,	SPECIES,	LENGTH_CLASSES_CODE,	WEIGHT_OF_THE_FRACTION,	WEIGHT_OF_THE_SAMPLE_MEASURED,	SEX,	NO_OF_INDIVIDUAL_OF_THE_ABOVE_SEX_MEASURED,	LENGTH_CLASS,	MATURITY,	MATSUB from Result Group by TYPE_OF_FILE,	COUNTRY,	AREA,	VESSEL,	YEAR,  MONTH,  DAY,	HAUL_NUMBER,	CODEND_CLOSING,	PART_OF_THE_CODEND,	FAUNISTIC_CATEGORY,GENUS,	SPECIES,	LENGTH_CLASSES_CODE,	WEIGHT_OF_THE_FRACTION,	WEIGHT_OF_THE_SAMPLE_MEASURED,	SEX,	NO_OF_INDIVIDUAL_OF_THE_ABOVE_SEX_MEASURED,	LENGTH_CLASS,	MATURITY,	MATSUB", sep="") 
       
    }
    #Matrix=sqlQuery(channel, query)
    Matrix=sqldf(query)
    #odbcClose(channel)
    Err=which(Matrix$count>1)
    if (length(Err)!=0){
      for (j in 1:length(Err)){
        m=Matrix[Err[j],]
        write(paste("Haul" , as.character(m$HAUL_NUMBER) , ", species ",  as.character(m$GENUS), as.character(m$SPECIES), ", sex", as.character(m$SEX) ,as.character(m$MATURITY), as.character(m$MATSUB),", length", m$LENGTH_CLASS, ": identical records in", Matrix$TYPE_OF_FILE[1]), file = Errors, append = TRUE)}
    }   else
    {
      write(paste("No error occurred"), file = Errors, append = TRUE)  
      check_without_errors = TRUE
    }
  }
  if (Type_of_files==".csv"){
#     unlink(paste(Data,".xls", sep = ""))
  }
  return(check_without_errors)
}
#######################################################################################################################
