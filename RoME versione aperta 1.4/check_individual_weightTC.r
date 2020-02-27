###########################################################################################################################
#   RoME: R code to perform multiple checks on MEDITS Survey data (TA, TB, TC and TE files - old and new MEDITS formats)  #                   
#   Authors: I. Bitetto, M.T. Facchini, M.T. Spedicato                                                                    #
#   Coispa Tecnologia & Ricerca - Stazione sperimentale per lo Studio delle Risorse del Mare                              #
#   If you have any comments or suggestions please contact the following e-mail address: bitetto@coispa.it                #                                                                                  
#   March 2013                                                                                                            #
################################################################################
# Check if weight of the sample in TC is consistent with length-weight relationship

check_individual_weightTC<- function (){
 TC = ResultDataTC #read.csv(paste(DataTC,".csv",sep=""), sep=";",header=TRUE)
 
  write(paste("
        ----------- check consistency total weight in the haul in TC - ",TC$YEAR[1]), file = Errors, append = TRUE)
  numberError = 0
  
 
   
  LW = read.csv(paste(Length_weight,".csv",sep=""), sep=";",header=TRUE)
  TC$mean_weight = NA
 
  for (i in 1:nrow(TC)){
    ab=LW[(LW$SPECIES== paste(TC$GENUS[i],TC$SPECIES[i])) &(as.character(LW$SEX)==as.character(TC$SEX[i])) & (LW$AREA==TC$AREA[1]),]
    
    if (nrow(ab)!=0){
      A= ab$a[1]
      B= ab$b[1]
      if(as.character(TC$LENGTH_CLASSES_CODE[i])=="m"){
        mean_length =TC$LENGTH_CLASS[i] +0.5  
      } else if (as.character(TC$LENGTH_CLASSES_CODE[i])=="0"){ # step: 0.5 cm
        mean_length =(TC$LENGTH_CLASS[i] +2.5)/10 
      } else if (as.character(TC$LENGTH_CLASSES_CODE[i])=="1"){ # step: 1 cm
        mean_length =(TC$LENGTH_CLASS[i] +5)/10 
      } 
      mean_weight = A*mean_length^B
      
      TC$mean_weight[i] = mean_weight * TC$NUMBER_OF_INDIVIDUALS_IN_THE_LENGTH_CLASS_AND_MATURITY_STAGE[i] # estimated weight
      #TC$mean_individual_weight[i] = mean_weight 
      
    
    }
    
  }
  
  TC_w= aggregate(TC$mean_weight, by=list(TC$HAUL_NUMBER,TC$GENUS, TC$SPECIES,TC$WEIGHT_OF_THE_SAMPLE_MEASURED), FUN="sum")
  colnames(TC_w) = c("HAUL_NUMBER","GENUS", "SPECIES","WEIGHT_OF_THE_SAMPLE_MEASURED","ESTIMATED_WEIGHT")
  
  
  TC_w$perc_diff =  (TC_w$WEIGHT_OF_THE_SAMPLE_MEASURED-TC_w$ESTIMATED_WEIGHT)/TC_w$ESTIMATED_WEIGHT*100
  
  
  
  for (i in 1:nrow(TC_w)){
  if (!is.na(TC_w$perc_diff[i]) & abs(TC_w$perc_diff[i])>50){
  numberError=numberError+1 
  }
  }
  
  if (numberError ==0) {
    write(paste("No error occurred"), file = Errors, append = TRUE)
  } else {
    
  write.table(TC_w,file=paste("Comparison_estimated_observed_weight_in_TC_",  TC$YEAR[1],".csv",sep=""),sep=";",row.names=F)
    
    write("For some hauls the difference between estimated and observed total weight is greater than 50%. Please verify in the file Comparison_estimated_observed_weight_in_TC.csv automatically produced in the working directory", file = Errors, append = TRUE)
  }
  return(TRUE)
  

}