###########################################################################################################################
#   RoME: R code to perform multiple checks on MEDITS Survey data (TA, TB, TC and TE files - old and new MEDITS formats)                                   #                   
#   Authors: I. Bitetto, M.T. Facchini, M.T. Spedicato                                                                    #
#   Coispa Tecnologia & Ricerca - Stazione sperimentale per lo Studio delle Risorse del Mare                              #
#   If you have any comments or suggestions please contact the following e-mail address: bitetto@coispa.it                #                                                                                  
#   March 2013                                                                                                            #
###########################################################################################################################
# Check if TA, TB and TC files have the same area and year

check_area_year<-function(){
 
  numberError = 0
  
  ResultTA = ResultDataTA #read.csv(paste(DataTA,".csv",sep=""), sep=";", header=TRUE)
  
  ResultTB = ResultDataTB #read.csv(paste(DataTB,".csv",sep=""), sep=";", header=TRUE)
  
  ResultTC = ResultDataTC #read.csv(paste(DataTC,".csv",sep=""), sep=";", header=TRUE)
  
  write(paste("
              ----------- check consistency of area and year TA, TB and TC - ", ResultTA$YEAR[1]), file = Errors, append = TRUE)
			  
  if (Format=="before_2012")  {
  if  (as.character(DataTD)!="")  {
  ResultTD = read.csv(paste(DataTD,".csv",sep=""), sep=";", header=TRUE)
  }
  if (as.character(DataTT)!=""){
  ResultTT = read.csv(paste(DataTT,".csv",sep=""), sep=";", header=TRUE)
  }
  }  else if (as.character(DataTE)!="") {

  ResultTE = read.csv(paste(DataTE,".csv",sep=""), sep=";", header=TRUE)
  }
  
  GSA_TA=unique(ResultTA$AREA)
  
  GSA_TB=unique(ResultTB$AREA)
  
  GSA_TC=unique(ResultTC$AREA)
  
  if (Format=="before_2012") {
  if  (as.character(DataTD)!="")  {
  GSA_TD=unique(ResultTD$AREA)
  }
  if (as.character(DataTT)!=""){
  GSA_TT=unique(ResultTT$AREA)
  }
  }  else if (as.character(DataTE)!="") {
  GSA_TE=unique(ResultTE$AREA)
  }
  
  year_TA=unique(ResultTA$YEAR)
  
  year_TB=unique(ResultTB$YEAR)
  
  year_TC=unique(ResultTC$YEAR)
  
  
  if (Format=="before_2012") {
  if  (as.character(DataTD)!="")  {
  year_TD=unique(ResultTD$YEAR)
  }
  if (as.character(DataTT)!=""){
  year_TT=unique(ResultTT$YEAR)
  }
  }  else if (as.character(DataTE)!=""){
  year_TE=unique(ResultTE$YEAR)
  }
  
#   if (((Format=="before_2012") &   (as.character(DataTT)=="") &  (as.character(DataTD)=="") )| (Format=="from_2012") & (DataTE!="")){
##CANCELLARE
if(is.null(Format)) print("DATATT nullo")
if(is.null(DataTT)) print("DATATT nullo")
if(is.null(DataTD)) print("DATATD nullo")

if (((Format=="before_2012") &   is.null(DataTT) &  is.null(DataTD) ) | (Format=="from_2012") & (DataTE!="")){
    if ( (GSA_TA != GSA_TB) | (GSA_TB != GSA_TC) | (GSA_TA != GSA_TC)){
      write(paste("Different value for field AREA in TA, TB and TC files"), file = Errors, append = TRUE)
      numberError = numberError +1
    }
    
    if ( (year_TA != year_TB) | (year_TB != year_TC) | (year_TA != year_TC)){
      write(paste("Different value for field YEAR in TA, TB and TC files"), file = Errors, append = TRUE)
      numberError = numberError +1
    }
  } else if ((Format=="before_2012") &   is.null(DataTD)  &  is.null(DataTT))  {
  if ( (GSA_TA != GSA_TB) | (GSA_TB != GSA_TC) | (GSA_TA != GSA_TC) | (GSA_TD != GSA_TA)| (GSA_TT != GSA_TA)| (GSA_TT != GSA_TD) | (GSA_TD != GSA_TB)| (GSA_TT != GSA_TB)| (GSA_TD != GSA_TC)| (GSA_TT != GSA_TC)){
    write(paste("Different value for field AREA in TA, TB, TC, TD and TT files"), file = Errors, append = TRUE)
    numberError = numberError +1
  }
  
  if ( (year_TA != year_TB) | (year_TB != year_TC) | (year_TA != year_TC)| (year_TD != year_TA)| (year_TT != year_TA)| (year_TT != year_TD) | (year_TD != year_TB)| (year_TT != year_TB)| (year_TD != year_TC)| (year_TT != year_TC)){
    write(paste("Different value for field YEAR in TA, TB, TC, TD and TT files"), file = Errors, append = TRUE)
    numberError = numberError +1
  }
} else if ((Format=="from_2012") & (DataTE!="")){
 if ( (GSA_TA != GSA_TB) | (GSA_TB != GSA_TC) | (GSA_TA != GSA_TC) | (GSA_TE != GSA_TA)| (GSA_TE != GSA_TB)| (GSA_TE != GSA_TC)){
    write(paste("Different value for field AREA in TA, TB, TC, TE files"), file = Errors, append = TRUE)
    numberError = numberError +1
  }
  
  if ( (year_TA != year_TB) | (year_TB != year_TC) | (year_TA != year_TC)| (year_TE != year_TB) | (year_TE != year_TC)){
    write(paste("Different value for field YEAR in TA, TB, TC and TE files"), file = Errors, append = TRUE)
    numberError = numberError +1
  }


} else if ((Format=="before_2012") &   (as.character(DataTD)=="")  &  (as.character(DataTT)!=""))  {
  if ( (GSA_TA != GSA_TB) | (GSA_TB != GSA_TC) | (GSA_TA != GSA_TC) | (GSA_TT != GSA_TA)| (GSA_TT != GSA_TB)| (GSA_TT != GSA_TC)){
    write(paste("Different value for field AREA in TA, TB, TC, TD and TT files"), file = Errors, append = TRUE)
    numberError = numberError +1
  }
  
  if ( (year_TA != year_TB) | (year_TB != year_TC) | (year_TA != year_TC)|(year_TT != year_TA)| (year_TT != year_TB)| (year_TT != year_TC)){
    write(paste("Different value for field YEAR in TA, TB, TC, TD and TT files"), file = Errors, append = TRUE)
    numberError = numberError +1
  }
} else if ((Format=="before_2012") &   (as.character(DataTD)!="")  &  (as.character(DataTT)==""))  {
  if ( (GSA_TA != GSA_TB) | (GSA_TB != GSA_TC) | (GSA_TA != GSA_TC) | (GSA_TD != GSA_TA)|  (GSA_TD != GSA_TB)| (GSA_TD != GSA_TC)){
    write(paste("Different value for field AREA in TA, TB, TC, TD and TT files"), file = Errors, append = TRUE)
    numberError = numberError +1
  }
  
  if ( (year_TA != year_TB) | (year_TB != year_TC) | (year_TA != year_TC)| (year_TD != year_TA)|  (year_TD != year_TB)| (year_TD != year_TC)){
    write(paste("Different value for field YEAR in TA, TB, TC, TD and TT files"), file = Errors, append = TRUE)
    numberError = numberError +1
  }
} 
  
  if (numberError ==0) {
    write(paste("No error occurred"), file = Errors, append = TRUE)
  }  
  
  if (numberError ==0) {
    return(TRUE)
  } else { return(FALSE) }
}

################################################################################