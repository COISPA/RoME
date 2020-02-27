###########################################################################################################################
#   RoME: R code to perform multiple checks on MEDITS Survey data (TA, TB, TC and TE files - old and new MEDITS formats)                                   #                   
#   Authors: I. Bitetto, M.T. Facchini, M.T. Spedicato                                                                    #
#   Coispa Tecnologia & Ricerca - Stazione sperimentale per lo Studio delle Risorse del Mare                              #
#   If you have any comments or suggestions please contact the following e-mail address: bitetto@coispa.it                #                                                                                  
#   March 2013
################################################################################
# Check if all the target species in TB are present in TC

check_species_TBTC<-function(DataSpecies,ResultTB,ResultTC){
  
#   library(RODBC)
  numberError = 0
  #ResultTC = read.csv(paste(DataTC,".csv",sep=""), sep=";", header=TRUE)
  
  #ResultTB = read.csv(paste(DataTB,".csv",sep=""), sep=";", header=TRUE)
  
  write(paste(" ----------- check presence in TC of TB target species - ",ResultTC$YEAR[1]), file = Errors, append = TRUE)
  
  
  
  ResultTB= ResultTB[,which(names(ResultTB)=="YEAR" | names(ResultTB)=="HAUL_NUMBER" | names(ResultTB)=="GENUS" | names(ResultTB)=="SPECIES")]
  ResultTC=ResultTC[,which(names(ResultTC)=="YEAR" | names(ResultTC)=="HAUL_NUMBER" | names(ResultTC)=="GENUS" | names(ResultTC)=="SPECIES")]
  
  ResultSpecies=read.csv(file=paste(DataSpecies,".csv",sep=""), sep=";", header=TRUE )
  
  if (nrow(ResultSpecies)!=0) {
    
    Start = is.na(ResultSpecies$TARGET_START) == FALSE
    End =  is.na(ResultSpecies$TARGET_END) == FALSE
    Target = ResultSpecies[1,]
    
    ntarget=1;
    for (l in 1:nrow(ResultSpecies)){
      
      if (Start[l] == TRUE)
      {
        if (End[l] == TRUE)
        {
          if ( 
            ( (ResultSpecies$TARGET_START[l]<=ResultTB$YEAR[1]) == TRUE) & 
              ( (ResultSpecies$TARGET_END[l]>ResultTB$YEAR[1]) == TRUE) )
          {
            Target[ntarget,]= ResultSpecies[l,]
            ntarget=ntarget+1
          } 
        }  else
        {
          if ( (ResultSpecies$TARGET_START[l]<=ResultTB$YEAR[1]) == TRUE)
          {
            Target[ntarget,]= ResultSpecies[l,]
            ntarget=ntarget+1
          }
        }
      }
    }
    
    if ( (nrow(ResultTB)!=0)){
      for (j in 1:nrow(ResultTB)){
        StrSpecies= paste(ResultTB$GENUS[j], ResultTB$SPECIES[j], sep="" )
        FoundTarget=Target[(Target$SPECIES==StrSpecies),]
        if (nrow(FoundTarget) != 0) {
          FoundInTC=ResultTC[as.character(ResultTC$GENUS)==as.character(ResultTB$GENUS[j]) & as.character(ResultTC$SPECIES)==as.character(ResultTB$SPECIES[j]) & ResultTC$HAUL_NUMBER==ResultTB$HAUL_NUMBER[j],]
          if (nrow(FoundInTC) == 0) {
            write(paste("Warning: Haul",ResultTB$HAUL_NUMBER[j],ResultTB$GENUS[j], ResultTB$SPECIES[j], "not found in TC"), file = Errors, append = TRUE)
          }
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
  
}
################################################################################