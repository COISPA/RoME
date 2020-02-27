###########################################################################################################################
#   RoME: R code to perform multiple checks on MEDITS Survey data (TA, TB, TC and TE files - old and new MEDITS formats)                                   #                   
#   Authors: I. Bitetto, M.T. Facchini, M.T. Spedicato                                                                    #
#   Coispa Tecnologia & Ricerca - Stazione sperimentale per lo Studio delle Risorse del Mare                              #
#   If you have any comments or suggestions please contact the following e-mail address: bitetto@coispa.it                #                                                                                  
#   March 2013                                                                                                            #
###########################################################################################################################
# Check if maturity stages in TC are consistent according to INSTRUCTION MANUAL VERSION 5 MEDITS 2007

check_mat_stages<-function(ResultDataTC, DataTargetSpecies){ 
  numberError = 0
  ResultData = ResultDataTC #read.csv(paste(Data,".csv",sep=""), sep=";", header=TRUE)
  write(paste(" ----------- check consistency of maturity stages in ",ResultData$TYPE_OF_FILE[1],"-", ResultData$YEAR[1]), file = Errors, append = TRUE)
  
  
  
  if (Format=="before_2012"){
  #if (ResultData$YEAR[1]<2012){
  cat_fau=read.csv(file=paste(DataTargetSpecies,".csv",sep=""),sep=";",header=TRUE)
  cat_fau=cat_fau[cat_fau$FAUNISTIC_CATEGORY!="",]
  } else {
  cat_fau=read.csv(file=paste(DataSpecies,".csv",sep=""),sep=";",header=TRUE)  
  cat_fau=cat_fau[cat_fau$CATFAU!="",]    
  }
  if ((as.character(ResultData$TYPE_OF_FILE[1])=="TC"))    {
  ResultData = ResultData[as.character(ResultData$MATURITY)!="ND",] 
  } else if ((as.character(ResultData$TYPE_OF_FILE[1])=="TE")){
  ResultData_ND = ResultData[as.character(ResultData$MATURITY)=="ND",] 
  if (nrow(ResultData_ND)!=0){
  write("Warning: in TE the records with maturity ND are not allowed.", file = Errors, append = TRUE) 
  }
  }
 stages=read.csv(file=paste(DataMatStages,".csv",sep=""),sep=";",header=TRUE)
  
  if(as.character(ResultData$TYPE_OF_FILE[1])=="TE"){
  stages=stages[stages$MEDITS_STAGE_from_2012!="NDND",]                                       # for TE the maturity stage ND are not allowed
  }
  if (nrow(ResultData)!=0){
  ResultData$species=paste(ResultData$GENUS,ResultData$SPECIES,sep="")
  if( (ResultData$YEAR[1] > 2006) )  {
  ResultData$maturity=ifelse(is.na(ResultData$MATSUB)==FALSE,paste(ResultData$MATURITY,ResultData$MATSUB,sep=""),ResultData$MATURITY)
  }  
  
  for (i in 1:nrow(ResultData)){
    if (Format=="before_2012"){
    #if (ResultData$YEAR[1]<2012){
    cat_fau_one = as.character(cat_fau[cat_fau$SPECIES==ResultData$species[i],]$FAUNISTIC_CATEGORY) # in that table bony fish and selechians are distinguished, while in FM list not.
    } else {
    cat_fau_one = as.character(cat_fau[cat_fau$MeditsCode==ResultData$species[i],]$CATFAU)
    }
    if (length(cat_fau_one)==1){      # !=0
      if( (ResultData$YEAR[i] > 2006) & (ResultData$YEAR[i] < 2012))  {
      cat_fau_one = ifelse (cat_fau_one=="Ae","S",cat_fau_one)
        #ResultData$maturity=ifelse(is.na(ResultData$MATSUB)==FALSE,paste(ResultData$MATURITY,ResultData$MATSUB,sep=""),ResultData$MATURITY)
        stages_err = stages[as.character(stages$FAUNISTIC_CATEGORY)==substring(cat_fau_one ,1,1)      & as.character(stages$SEX)== as.character(ResultData$SEX[i])  & as.character(stages$MEDITS_STAGE)== as.character(ResultData$maturity[i]),]
      }  else if ((ResultData$YEAR[i] <= 2006)) {  
        cat_fau_one = ifelse (cat_fau_one=="Ae","S",cat_fau_one)
        stages_err = stages[as.character(stages$FAUNISTIC_CATEGORY)==substring(cat_fau_one ,1,1) & as.character(stages$SEX)== as.character(ResultData$SEX[i]) & as.character(stages$MEDITS_STAGE_up_to_2006)== as.character(ResultData$MATURITY[i]),]
      }   else if (ResultData$YEAR[i] >= 2012 & Format == "from_2012"){
      stages_err = stages[as.character(stages$FAUNISTIC_CATEGORY)==cat_fau_one     & as.character(stages$SEX)== as.character(ResultData$SEX[i])  & as.character(stages$MEDITS_STAGE_from_2012)== as.character(ResultData$maturity[i]),]
      }   else if (ResultData$YEAR[i] >= 2012 & Format == "before_2012"){
      stages_err = stages[as.character(stages$FAUNISTIC_CATEGORY)==cat_fau_one     & as.character(stages$SEX)== as.character(ResultData$SEX[i])  & as.character(stages$MEDITS_STAGE)== as.character(ResultData$maturity[i]),]
      }
        
      if (nrow(stages_err) == 0
          #particular case of NEPRNOR and sex 0 for males crustaceans before 2006
          |  ( (cat_fau_one == "B") & (ResultData$species[i] != "NEPRNOR") & (as.character(ResultData$SEX[i]) == "F") & (ResultData$MATURITY[i] == 3)) )  
          
      {   
      
      if ( (cat_fau_one == "B") & (as.character(ResultData$SEX[i]) == "M")  & ( (ResultData$MATURITY[i] == 0) | is.na(ResultData$MATURITY[i]) ) & (ResultData$YEAR[i]> 2006))     {
        if (is.na(ResultData$MATSUB[i])){
          write(paste("Warning: Haul",ResultData$HAUL_NUMBER[i],ResultData$species[i], ResultData$SEX[i] , 
                      ResultData$LENGTH_CLASS[i], ResultData$MATURITY[i],"FAUNISTIC_CATEGORY, SEX and MATURITY inconsistent according to MEDITS INSTRUCTIONS MANUAL in", ResultData$TYPE_OF_FILE[1]), file = Errors, append = TRUE)  } else 
                      {   
                        write(paste("Haul",ResultData$HAUL_NUMBER[i],ResultData$species[i], ResultData$SEX[i] , 
                                    ResultData$LENGTH_CLASS[i], ResultData$MATURITY[i],ResultData$MATSUB[i],"FAUNISTIC_CATEGORY, SEX and MATURITY inconsistent according to MEDITS INSTRUCTIONS MANUAL in", ResultData$TYPE_OF_FILE[1]), file = Errors, append = TRUE)
                                    } 
        
      }
      
       if (is.na(ResultData$MATSUB[i])){
          write(paste("Warning: Haul",ResultData$HAUL_NUMBER[i],ResultData$species[i], ResultData$SEX[i] , 
                      ResultData$LENGTH_CLASS[i], ResultData$MATURITY[i],"FAUNISTIC_CATEGORY, SEX and MATURITY inconsistent according to MEDITS INSTRUCTIONS MANUAL in", ResultData$TYPE_OF_FILE[1]), file = Errors, append = TRUE)  } else 
                      {   
                        write(paste("Haul",ResultData$HAUL_NUMBER[i],ResultData$species[i], ResultData$SEX[i] , 
                                    ResultData$LENGTH_CLASS[i], ResultData$MATURITY[i],ResultData$MATSUB[i],"FAUNISTIC_CATEGORY, SEX and MATURITY inconsistent according to MEDITS INSTRUCTIONS MANUAL in", ResultData$TYPE_OF_FILE[1]), file = Errors, append = TRUE)
                      }
                                    
          
      }
      
    }   else {
      write(paste(ResultData$species[i], ": in MATURITY_STAGES table (Tables directory) is not set the faunistic category. For checking the maturity stages, fill in the field FAUNISTIC_CATEGORY in that table."), file = Errors, append = TRUE) 
    }     # ciclo catfau non nullo
  }            # ciclo for
  
  }
  
  if (numberError ==0) {
    write(paste("No error occurred"), file = Errors, append = TRUE)
  }	
  
  if (numberError ==0) {
    return(TRUE)
  } else { return(FALSE) }
  
  
}
################################################################################