
###########################################################################################################################
#   RoME: R code to perform multiple checks on MEDITS Survey data (TA, TB, TC and TE files - old and new MEDITS formats)                                   #                   
#   Authors: I. Bitetto, M.T. Facchini, M.T. Spedicato                                                                    #
#   Coispa Tecnologia & Ricerca - Stazione sperimentale per lo Studio delle Risorse del Mare                              #
#   If you have any comments or suggestions please contact the following e-mail address: bitetto@coispa.it                #                                                                                  
#   March 2013                                                                                                            #
###########################################################################################################################
# Creation of R-SUFI files:
# capt.csv
create_catch<-function(ResultDataTB){
  ResultData = ResultDataTB  #read.csv(paste(DataTB,".csv",sep=""), sep=";", header=TRUE)
  
  capt=matrix(nrow=nrow(ResultData),ncol=6)
  colnames(capt)=(c("Survey",  "Year",	"Haul",	"Species",	"Number",	"Weight") )
  capt[,1]=paste("MEDITS-GSA",as.character(ResultData$AREA[1]),sep="")
  capt[,2]=as.character(ResultData$YEAR[1])
  capt[,3]=ResultData$HAUL_NUMBER
  capt[,4]=paste(ResultData$GENUS,ResultData$SPECIES,sep="")
  if(Format=="before_2012") {
    capt[,5]=as.numeric(as.character(ResultData$TOTAL_NUMBER_IN_HAUL))
  } else {
    capt[,5]=as.numeric(as.character(ResultData$TOTAL_NUMBER_IN_THE_HAUL))}
  
  if(Format=="before_2012"){
  capt[,6]=round(ResultData$TOTAL_WEIGHT_IN_HAUL/1000,3)
  } else {
  capt[,6]=round(ResultData$TOTAL_WEIGHT_IN_THE_HAUL/1000,3)  
  }
#   write.csv(capt,file=paste("files R-Sufi/captures_",Year,"_GSA",ResultData$AREA[1],".csv",sep=""),row.names=FALSE,quote=FALSE,sep=";") 

write.table(capt,file=paste("files R-Sufi/captures_",ResultData$YEAR[1],"_GSA",ResultData$AREA[1],".csv",sep=""),row.names=FALSE,quote=FALSE,sep=";") 
}
###########################################################################################################################