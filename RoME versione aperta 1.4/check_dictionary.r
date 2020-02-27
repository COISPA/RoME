###########################################################################################################################
#   RoME: R code to perform multiple checks on MEDITS Survey data (TA, TB, TC and TE files - old and new MEDITS formats)                                   #                   
#   Authors: I. Bitetto, M.T. Facchini, M.T. Spedicato                                                                    #
#   Coispa Tecnologia & Ricerca - Stazione sperimentale per lo Studio delle Risorse del Mare                              #
#   If you have any comments or suggestions please contact the following e-mail address: bitetto@coispa.it                #                                                                                  
#   March 2013                                                                                                            #
###########################################################################################################################
# Check if the values in several fields belong to the allowed ranges according to INSTRUCTION MANUAL VERSION 5 MEDITS 2007 

check_dictionary<-function(ResultData,Field,Values){
  numberError = 0

  Result = ResultData # read.csv(paste(Data,".csv",sep=""), sep=";", header=TRUE)
    write(paste("
              ----------- check dictionary for field:", Field, "-", Result$YEAR[1]), file = Errors, append = TRUE)
			  
  Valuesf <- factor(Values)
  if ( (Result$TYPE_OF_FILE[1] == "TA") & (Field=="CODEND_CLOSING") ){
    Result=Result[(Result$CODEND_CLOSING != "") & is.na(Result$CODEND_CLOSING) == FALSE,]
  }
  
  if ( (Result$TYPE_OF_FILE[1] == "TA") & (Field=="COURSE") ){
    Result=Result[(Result$COURSE != "") & is.na(Result$COURSE) == FALSE ,]
  }
  if ( (Result$TYPE_OF_FILE[1] == "TA") & (Field=="GEOMETRICAL_PRECISION") ){
    Result=Result[(Result$GEOMETRICAL_PRECISION != "") & is.na(Result$GEOMETRICAL_PRECISION) == FALSE ,]
  }
  
  indexcol= which(names(Result)==Field)
  
  if ( (nrow(Result)!=0)){
    for (k in 1:nrow(Result)){
      if ((is.na(as.character(Result[k,indexcol]))==TRUE )| (as.character(Result[k,indexcol])=="")){
        if (Result$TYPE_OF_FILE[1] == "TA") {
          if (Result$VALIDITY[k]=="V") {
            if ((Format=="from_2012") & !is.na(as.character(Result$BOTTOM_TEMPERATURE_BEGINNING[k])) & !is.na(as.character(Result$BOTTOM_TEMPERATURE_END[k])) ){
            write(paste("Haul",as.character(Result$HAUL_NUMBER[k]), ": the field", Field, "is empty in",  Result$TYPE_OF_FILE[1]), file = Errors, append = TRUE)
            numberError = numberError +1
            }
          }
          } else if (Result$TYPE_OF_FILE[1] == "TB") {
                write(paste("Haul",Result$HAUL_NUMBER[k],Result$GENUS[k], Result$SPECIES[k], ": the field", Field, "is empty in",  Result$TYPE_OF_FILE[1]), file = Errors, append = TRUE)
                numberError = numberError +1} else  #TC
                { 
                  write(paste("Haul",Result$HAUL_NUMBER[k], Result$GENUS[k], Result$SPECIES[k], Result$SEX[k], Result$LENGTH_CLASS[k], ": the field", Field, "is empty in",  Result$TYPE_OF_FILE[1]), file = Errors, append = TRUE)
                  numberError = numberError +1
                
                }
      } else {
        
        if (any(as.character(Result[k,indexcol])==Valuesf) == FALSE) {   
          if (Result$TYPE_OF_FILE[1] == "TA") {
            write(paste("Haul",Result$HAUL_NUMBER[k], ": value not allowed for", Field, "in",  Result$TYPE_OF_FILE[1]), file = Errors, append = TRUE)
            numberError = numberError +1
            
          } else if (Result$TYPE_OF_FILE[1] == "TB") {
            
            write(paste("Haul",Result$HAUL_NUMBER[k],Result$GENUS[k], Result$SPECIES[k], ": value not allowed for", Field, "in",  Result$TYPE_OF_FILE[1]), file = Errors, append = TRUE)
            numberError = numberError +1
            
          } else { # TC
          if ((Field!="SEX")) {
                
           # if ((Field=="SEX")&(as.character(Result[k,indexcol])!="FALSE")) {
            write(paste("Haul",Result$HAUL_NUMBER[k],Result$GENUS[k], Result$SPECIES[k], Result$SEX[k], Result$LENGTH_CLASS[k],  ": value not allowed for", Field, "in",  Result$TYPE_OF_FILE[1] ), file = Errors, append = TRUE)
            numberError = numberError +1
          } else {
             if ((as.character(Result[k,indexcol])!="FALSE")) {
                 write(paste("Haul",Result$HAUL_NUMBER[k],Result$GENUS[k], Result$SPECIES[k], Result$SEX[k], Result$LENGTH_CLASS[k],  ": value not allowed for", Field, "in",  Result$TYPE_OF_FILE[1] ), file = Errors, append = TRUE)
            numberError = numberError +1
             }
          }
          #  if ((Field=="SEX")&(as.character(Result[k,indexcol])!="FALSE")){
#            write(paste("Haul",Result$HAUL_NUMBER[k],Result$GENUS[k], Result$SPECIES[k], Result$SEX[k], Result$LENGTH_CLASS[k],  ": value not allowed for", Field, "in",  Result$TYPE_OF_FILE[1] ), file = Errors, append = TRUE)
#            numberError = numberError +1
#          } 
          
          
          }
        }
      }}
  }
  
  if (numberError ==0) {
    write(paste("No error occurred for field", Field, "in",  Result$TYPE_OF_FILE[1]), file = Errors, append = TRUE)
  }  
  
  if (numberError ==0) {
    return(TRUE)
  } else { return(FALSE) }
  
}
################################################################################