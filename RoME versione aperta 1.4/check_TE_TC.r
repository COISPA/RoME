###########################################################################################################################
#   RoME: R code to perform multiple checks on MEDITS Survey data (TA, TB, TC and TE files - old and new MEDITS formats)                                   #                   
#   Authors: I. Bitetto, M.T. Facchini, M.T. Spedicato                                                                    #
#   Coispa Tecnologia & Ricerca - Stazione sperimentale per lo Studio delle Risorse del Mare                              #
#   If you have any comments or suggestions please contact the following e-mail address: bitetto@coispa.it                #                                                                                  
#   March 2013                                                                                                            #
###########################################################################################################################
# Check about the consistency of the number of individuals by length, sex and stage between TC and TE

check_TE_TC <- function (){
  numberError = 0
  TC = ResultDataTC #read.csv(paste(DataTC,".csv",sep=""), sep=";", header=TRUE) 
  TE = ResultDataTE #read.csv(paste(DataTE,".csv",sep=""), sep=";", header=TRUE)

if (nrow(TE[which(TE$MATSUB=="O"),])!=0){  
  TE[which(TE$MATSUB=="O"),]$MATSUB="ND"
}
  
  write(paste("
----------- check consistency nb of individuals TC and TE - ",TC$YEAR[1]), file = Errors, append = TRUE)
   for (i in 1:nrow(TE)){
  TE$SEX[i]=ifelse(as.character(TE$SEX[i])=="FALSE","F",as.character(TE$SEX[i]))
  }
  
  for (i in 1:nrow(TE)){
  # check if the individual in TE is in TC
    TC_temp = TC[which((TC$HAUL_NUMBER==TE$HAUL_NUMBER[i])& (as.character(TC$GENUS)==as.character(TE$GENUS[i])) & (as.character(TC$SPECIES)==as.character(TE$SPECIES[i])) & (as.character(TC$SEX)==as.character(TE$SEX[i])) & (TC$LENGTH_CLASS==TE$LENGTH_CLASS[i]) & (as.character(TC$MATURITY)==as.character(TE$MATURITY[i])) & (as.character(TC$MATSUB)==as.character(TE$MATSUB[i]))),]
   nb_TC= TC_temp[,ncol(TC)]
    
    if (nrow(TC_temp)==0) { # record not present in TC
  write(paste("Warning: Haul ",TE$HAUL_NUMBER[i],as.character(TE$GENUS[i]),as.character(TE$SPECIES[i]),", sex ",ifelse(as.character(TE$SEX[i])=="FALSE","F",as.character(TE$SEX[i])),", length ",TE$LENGTH_CLASS[i],"mm, maturity",as.character(TE$MATURITY[i]),as.character(TE$MATSUB[i])," : record not present in TC"), file = Errors, append = TRUE) 
  #numberError=numberError+1
  
  } else { # record present: check on the number (must be <= the number in TC)
  # sum of individuals in TE:
    nb_TE = nrow(TE[which((TE$HAUL_NUMBER==TE$HAUL_NUMBER[i])& (as.character(TE$GENUS)==as.character(TE$GENUS[i]))& (as.character(TE$SPECIES)==as.character(TE$SPECIES[i]))& (as.character(TE$SEX)==as.character(TE$SEX[i]))& (TE$LENGTH_CLASS==TE$LENGTH_CLASS[i])& (as.character(TE$MATURITY)==as.character(TE$MATURITY[i]))& (as.character(TE$MATSUB)==as.character(TE$MATSUB[i]))),])
   if (nb_TC<nb_TE){
     write(paste("Warning: Haul ",TE$HAUL_NUMBER[i],as.character(TE$GENUS[i]),as.character(TE$SPECIES[i]),", sex ",ifelse(as.character(TE$SEX[i])=="FALSE","F",as.character(TE$SEX[i])),", length ",TE$LENGTH_CLASS[i],"mm, maturity",as.character(TE$MATURITY[i]),as.character(TE$MATSUB[i])," : the number of individuals in TE (=",nb_TE,") is greater than the number reported in TC(=",nb_TC,")"), file = Errors, append = TRUE) 
    numberError=numberError+1    
   }
  }
  
  }
      if (numberError ==0) {
    write(paste("No error occurred"), file = Errors, append = TRUE)
   }
  if (numberError ==0) {
    return(TRUE)
   } else { return(FALSE) 
   }
  
}