###########################################################################################################################
#   RoME: R code to perform multiple checks on MEDITS Survey data (TA, TB, TC and TE files - old and new MEDITS formats)                                   #                   
#   Authors: I. Bitetto, M.T. Facchini, M.T. Spedicato                                                                    #
#   Coispa Tecnologia & Ricerca - Stazione sperimentale per lo Studio delle Risorse del Mare                              #
#   If you have any comments or suggestions please contact the following e-mail address: bitetto@coispa.it                #                                                                                  
#   March 2013                                                                                                            #
###########################################################################################################################
# Check corretness of association between category and sub-category in TL consistent according to INSTRUCTION MANUAL VERSION 9 
# MEDITS 2017

check_associations_category_TL<-function(ResultDataTL,AssociationsTL){ 
  numberError = 0
  ResultData = ResultDataTL #read.csv(paste(Data,".csv",sep=""), sep=";", header=TRUE)
  write(paste(" ----------- check consistency of category/subcategory codes in ",ResultData$TYPE_OF_FILE[1],"-", ResultData$YEAR[1]), file = Errors, append = TRUE)
  
  
  assTL=read.table(file=paste(AssociationsTL,".csv",sep=""),sep=";",header=TRUE)  
     
 

 #stages=read.csv(file=paste(DataMatStages,".csv",sep=""),sep=";",header=TRUE)
  
 # if(as.character(ResultData$TYPE_OF_FILE[1])=="TE"){
  #stages=stages[stages$MEDITS_STAGE_from_2012!="NDND",]                                       # for TE the maturity stage ND are not allowed
 # }
  if (nrow(ResultData)!=0){
  #ResultData$species=paste(ResultData$GENUS,ResultData$SPECIES,sep="")
  #if( (ResultData$YEAR[1] > 2006) )  {
 # ResultData$maturity=ifelse(is.na(ResultData$MATSUB)==FALSE,paste(ResultData$MATURITY,ResultData$MATSUB,sep=""),ResultData$MATURITY)
  #}  
  
  for (i in 1:nrow(ResultData)){
  
    ass_allowed_TL = as.character(assTL[assTL$LITTER_CATEGORY==ResultData$LITTER_CATEGORY[i],]$LITTER_SUB-CATEGORY)
    
    if (!(as.character(ResultData$LITTER_SUB-CATEGORY[i]) %in% ass_allowed_TL)){
      numberError=numberError+1
      write(paste(ResultData$YEAR[i], " " ,ResultData$HAUL_NUMBER[i], ": Association between category and sub-category not allowed."), file = Errors, append = TRUE) 
  
    }
    
    
  if (numberError ==0) {
    write(paste("No error occurred"), file = Errors, append = TRUE)
  }	
  
  } 
  }
  
    return(TRUE)
}
  
  

################################################################################