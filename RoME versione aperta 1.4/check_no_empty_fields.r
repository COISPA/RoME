###########################################################################################################################
#   RoME: R code to perform multiple checks on MEDITS Survey data (TA, TB, TC and TE files - old and new MEDITS formats)                                   #                   
#   Authors: I. Bitetto, M.T. Facchini, M.T. Spedicato                                                                    #
#   Coispa Tecnologia & Ricerca - Stazione sperimentale per lo Studio delle Risorse del Mare                              #
#   If you have any comments or suggestions please contact the following e-mail address: bitetto@coispa.it                #                                                                                  
#   March 2013                                                                                                            #
###########################################################################################################################
# Check empty fields in TA,TB,TC

check_no_empty_fields<-function(ResultData,Table){
  numberError = 0
  
  Matrix = ResultData #read.csv(paste(Data,".csv",sep=""), sep=";", header=TRUE)
  if ( (Table == "TA") == TRUE)
  {
    write(paste("
                ----------- check no empty fields"), file = Errors, append = TRUE) 
    write(paste("TA - ",Matrix$YEAR[1]), file = Errors, append = TRUE)
    
    Matrix=Matrix[Matrix$VALIDITY=="V",]
    
    if (Format=="before_2012"){
    Mat=Matrix[,1:33]  
    } else {
    Mat=Matrix[,c(1:34)]
    }
  } else if ( (Table == "TB") == TRUE){
    write(paste("TB- ",Matrix$YEAR[1]), file = Errors, append = TRUE)
    Mat=Matrix
  } else if ( (Table == "TC") == TRUE) {
    write(paste("TC- ",Matrix$YEAR[1]), file = Errors, append = TRUE)
    if (Format=="before_2012"){
    Mat=Matrix[,c(1:17,19)]
  } else {
    Mat=Matrix[,c(1:20,22)]  
  }
  } else if ( (Table == "TE") == TRUE){
    write(paste("TE- ",Matrix$YEAR[1]), file = Errors, append = TRUE)
    Mat=Matrix[,1:23]
  } 
  else if ( (Table == "TL") == TRUE){
    write(paste("TL- ",Matrix$YEAR[1]), file = Errors, append = TRUE)
    Mat=Matrix[,c(1:10,12,14)]
  } 
  
  empty_X=which(is.na(Mat)==TRUE,arr.ind=TRUE) 
   
  if (nrow(empty_X)!=0) {
    for (i in 1:nrow(empty_X)){
      # Exception for PART_OF_THE_CODEND field, according to MEDITS manual 2012
      if (names(Mat)[empty_X[i,2]]!="PART_OF_THE_CODEND"){   
        write(paste("Haul ",Mat$HAUL_NUMBER[empty_X[i,1]],"no value for ", names(Mat)[empty_X[i,2]]," in", Matrix$TYPE_OF_FILE[1]), file = Errors, append = TRUE)
        numberError = numberError + 1
      } else if (Mat$CODEND_CLOSING[empty_X[i,1]+1] == "C") {
        write(paste("Haul ",Mat$HAUL_NUMBER[empty_X[i,1]],"no value for ", names(Mat)[empty_X[i,2]]," in", Matrix$TYPE_OF_FILE[1]), file = Errors, append = TRUE)
        numberError = numberError + 1
      } 
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