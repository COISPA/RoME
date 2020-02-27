###########################################################################################################################
#   RoME: R code to perform multiple checks on MEDITS Survey data (TA, TB, TC and TE files - old and new MEDITS formats)  #                   
#   Authors: I. Bitetto, M.T. Facchini, M.T. Spedicato                                                                    #
#   Coispa Tecnologia & Ricerca - Stazione sperimentale per lo Studio delle Risorse del Mare                              #
#   If you have any comments or suggestions please contact the following e-mail address: bitetto@coispa.it                #                                                                                  
#   March 2013                                                                                                            #
################################################################################
# Check if in TB there are the total number, number of females, males and undetermined for species G1

check_nm_TB<- function (){
 TB = ResultDataTB #read.csv(paste(DataTB,".csv",sep=""), sep=";",header=TRUE)  
  TC = ResultDataTB #read.csv(paste(DataTC,".csv",sep=""), sep=";",header=TRUE)  
  write(paste("
              ----------- check presence of number of individuals for species G1 - ",TB$YEAR[1]), file = Errors, append = TRUE)
  numberError = 0
  
 
  
  #SELECTION OF SPECIES G1
  TB=TB   [(as.character(TB$FAUNISTIC_CATEGORY)=="Ae" )| 
          (as.character(TB$GENUS)=="MERL" & as.character(TB$SPECIES)=="MERL") |
          (as.character(TB$GENUS)=="MULL" ) |
          (as.character(TB$GENUS)=="ARIS" & as.character(TB$SPECIES)=="FOL") |
          (as.character(TB$GENUS)=="ARIT" & as.character(TB$SPECIES)=="ANT") |
          (as.character(TB$GENUS)=="ILLE" & as.character(TB$SPECIES)=="COI") |
          (as.character(TB$GENUS)=="LOLI" & as.character(TB$SPECIES)=="VUL") |
          (as.character(TB$GENUS)=="NEPR" & as.character(TB$SPECIES)=="NOR") |
          (as.character(TB$GENUS)=="PAPE" & as.character(TB$SPECIES)=="LON") ,]
  
  for (i in 1:nrow(TB)){
    if ((as.character(TB$NB_OF_FEMALES[i])=="0")&(as.character(TB$NB_OF_MALES[i])=="0")){
       TC_temp = TC[(TC$HAUL_NUMBER== TB$HAUL_NUMBER[i]) 
               & (as.character(TC$GENUS)== as.character(TB$GENUS[i]))
               & (as.character(TC$SPECIES)== as.character(TB$SPECIES[i])),]
               
       if (!(all(as.character(TC_temp$SEX)=="N" | as.character(TC_temp$SEX)=="I"))){
        write(paste("Haul", TB$HAUL_NUMBER[i],TB$GENUS[i],TB$SPECIES[i], "for the fields related to numbers of individuals, the value 0 is not allowed from 2012 for species G1"), file = Errors, append = TRUE) 
        numberError =   numberError +1 
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
 